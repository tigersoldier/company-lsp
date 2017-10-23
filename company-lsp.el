;;; company-lsp.el --- Company completion backend for lsp-mode.

;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "2.0") (company "0.9.0") (s "1.2.0"))
;; URL: https://github.com/tigersoldier/company-lsp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `company-lsp' is a `company' completion backend for `lsp-mode'.
;; To use it, add `company-lsp' to `company-backends':

;;     (require 'company-lsp)
;;     (push 'company-lsp company-backends)

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'lsp-mode)
(require 's)

(defgroup company-lsp nil
  "Company completion backend for lsp-mode."
  :prefix "company-lsp-"
  :group 'tools)

(defcustom company-lsp-cache-candidates t
  "Whether or not to cache completion candidates.

When set to non-nil, company caches the completion candidates so
company filters the candidates as completion progresses. If set
to nil, each incremental completion triggers a completion request
to the language server."
  :type 'boolean
  :group 'company-lsp)

(defun company-lsp--trigger-characters ()
  "Return a list of completion trigger characters specified by server."
  (when-let (completionProvider (lsp--capability "completionProvider"))
    (gethash "triggerCharacters" completionProvider)))

(defun company-lsp--completion-prefix ()
  "Return the completion prefix.

Return value is compatible with the `prefix' command of a company backend.

Return nil if no completion should be triggered. Return a string
as the prefix to be completed, or a cons cell of (prefix . t) to bypass
`company-minimum-prefix-length' for trigger characters."
  (if-let (trigger-chars (company-lsp--trigger-characters))
      (let* ((max-trigger-len (apply 'max (mapcar (lambda (trigger-char)
                                                    (length trigger-char))
                                                  trigger-chars)))
             (trigger-regex (s-join "\\|" trigger-chars))
             (symbol-cons (company-grab-symbol-cons trigger-regex max-trigger-len)))
        ;; Some major modes define trigger characters as part of the symbol. For
        ;; example "@" is considered a vaild part of symbol in java-mode.
        ;; Company will grab the trigger character as part of the prefix while
        ;; the server doesn't. Remove the leading trigger character to solve
        ;; this issue.
        (let* ((symbol (if (consp symbol-cons)
                           (car symbol-cons)
                         symbol-cons))
               (trigger-char (seq-find (lambda (trigger-char)
                                         (s-starts-with? trigger-char symbol))
                                       trigger-chars)))
          (if trigger-char
              (cons (substring symbol (length trigger-char)) t)
            symbol-cons)))
    (company-grab-symbol)))

(defun company-lsp--make-candidate (item)
  "Convert a CompletionItem JSON data to a string.

ITEM is a hashtable representing the CompletionItem interface.

The returned string has a lsp-completion-item property with the
value of ITEM."
  (propertize (gethash "label" item) 'lsp-completion-item item))

(defun company-lsp--insert-completion-text (candidate)
  "Replace a CompletionItem's label with its insertText.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (let* ((item (plist-get (text-properties-at 0 candidate) 'lsp-completion-item))
         (label (gethash "label" item))
         (start (- (point) (length label)))
         (insert-text (gethash "insertText" item)))
    (when insert-text
      (cl-assert (string-equal (buffer-substring-no-properties start (point)) label))
      (goto-char start)
      (delete-char (length label))
      (insert insert-text))))

(defvar-local company-lsp-response nil
  "Response from the server")

(defvar-local company-lsp-callback nil
  "Callback from company")

(defvar-local company-lsp-ids nil
  "List of request's id that should be processed")

(defvar-local company-lsp-mutex (make-mutex "COMPANY-LSP") "")
(defvar-local company-lsp-cond-var (make-condition-variable company-lsp-mutex "COMPANY-LSP") "")

(defun company-lsp-on-message (p msg)
  "Function called just after `lsp--parser-on-message' using `advice-add'.
It is executed in the main thread.
This hook processes all the responses and filter the ones with an id that is in
`company-lsp-ids'.
When a response is received, it informs the thread created that it can
processes the response.
P and MSG are the parameters from `lsp--parser-on-message'."
  (let* ((json-array-type 'list)
	 (json-object-type 'hash-table)
	 (json-false nil)
	 (json-data (json-read-from-string msg)))
    (pcase (lsp--get-message-type json-data)
      ('response (let ((id-msg (gethash "id" json-data nil)))
		   (with-mutex company-lsp-mutex
		     (when (member id-msg company-lsp-ids)
		       ;; If a response is received before our sub thread read the interesting one,
		       ;; the response is overwritten and it would be lost.
		       ;; So we save it
		       (setq company-lsp-response (lsp--parser-response-result p))
		       ;; Inform the thread that it can read the response
		       (condition-notify company-lsp-cond-var))))))))

(advice-add 'lsp--parser-on-message :after 'company-lsp-on-message)

(defun company-lsp-thread-process (id)
  "Function executed in a sub thread.
It waits for the response from the main thread and then call the callback.
It processes only the response that has an id equal to ID."
  (let ((id-sent id) response)
    (with-mutex company-lsp-mutex
      ;; Add our request id to a list.
      ;; When a response is received when can filter the interesting ones
      (push id-sent company-lsp-ids)
      ;; As the response is processed in the main thread, we wait for it.
      (condition-wait company-lsp-cond-var)
      ;; Process only the last user-requested completion
      (when (or (null company-lsp-ids)
		(= id-sent (car company-lsp-ids)))
	;; Notify all the others thread that they can exit.
	;; It shouldn't be necessary, but if the language server crashes and doesn't send a
	;; response, a thread could still wait for it.
	(condition-notify company-lsp-cond-var t)
	(setq response company-lsp-response)
	(setq company-lsp-ids nil)))
    (when-let* ((items (cond ((hash-table-p response) (gethash "items" response nil))
			     ((sequencep response) response))))
      (funcall company-lsp-callback (mapcar #'company-lsp--make-candidate
					    (lsp--sort-completions items))))))

(defun company-lsp-thread (cb)
  "Sends the requests and launch a thread that wait for the response.
CB is the callback that company gives us."
  ;; Save the callback to access it from the other thread.
  ;; I suppose it's always the same callback, otherwise it should be
  ;; give as parameter to company-lsp-thread-process
  (setq company-lsp-callback cb)
  (lsp--send-changes lsp--cur-workspace)
  ;; Call lsp--send-request with no-wait set to t
  (lsp--send-request (lsp--make-request
		      "textDocument/completion"
		      (lsp--text-document-position-params))
		     t)
  (lexical-let ((id (lsp--workspace-last-id lsp--cur-workspace)))
    ;; Save the request id and give it to the new thread
    (make-thread (lambda nil (company-lsp-thread-process id)) "THREAD-LSP")))

;;;###autoload
(defun company-lsp (command &optional arg &rest _)
  "Define a company backend for lsp-mode.

See the documentation of `company-backends' for COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-lsp))
    (prefix (and
             (bound-and-true-p lsp-mode)
             (lsp--capability "completionProvider")
             (or (company-lsp--completion-prefix) 'stop)))
    (candidates
     (cons :async
	   (if (>= emacs-major-version 26) #'company-lsp-thread
	     #'(lambda (callback)
		 (lsp--send-changes lsp--cur-workspace)
		 (let* ((resp (lsp--send-request (lsp--make-request
						  "textDocument/completion"
						  (lsp--text-document-position-params))))
			(items (cond
				((null resp) nil)
				((hash-table-p resp) (gethash "items" resp nil))
				((sequencep resp) resp))))
		   (funcall callback (mapcar #'company-lsp--make-candidate
					     (lsp--sort-completions items))))))))
    (sorted t)
    (no-cache (not company-lsp-cache-candidates))
    (annotation (lsp--annotate arg))
    (match (length arg))
    (post-completion (company-lsp--insert-completion-text arg))))

(provide 'company-lsp)
;;; company-lsp.el ends here
