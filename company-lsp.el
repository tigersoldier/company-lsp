;;; company-lsp.el --- Company completion backend for lsp-mode.  -*- lexical-binding: t -*-

;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.1") (company "0.9.0") (s "1.2.0") (dash "2.11.0"))
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
(require 'dash)

(defgroup company-lsp nil
  "Company completion backend for lsp-mode."
  :prefix "company-lsp-"
  :group 'tools)

(defcustom company-lsp-cache-candidates 'auto
  "Whether or not to cache completion candidates.

When set to non-nil, company caches the completion candidates so
company filters the candidates as completion progresses. If set
to nil, each incremental completion triggers a completion request
to the language server."
  :type '(choice (const :tag "Respect server response" auto)
                 (const :tag "Always cache" t)
                 (const :tag "Never cache" nil))
  :group 'company-lsp)

(defcustom company-lsp-async nil
  "Whether or not to use async operations to fetch data."
  :type 'boolean
  :group 'company-lsp)

(defcustom company-lsp-enable-snippet t
  "Whether or not to support expanding completion snippet.

If set to non-nil, company-lsp will register client capabilities
for snippet support. When the server returns completion item with
snippet, company-lsp will replace the label of the completion
item with the snippet and use yas-snippet to expand it."
  :type 'boolean
  :group 'company-lsp)

(declare-function yas-expand-snippet "ext:yasnippet.el")

(defvar company-lsp--snippet-functions '(("rust" . company-lsp--rust-completion-snippet))
  "Alist of functions to insert our snippets for each language.")

(defvar-local company-lsp--completion-cache nil
  "Cached completion. It's an alist of (prefix . completion).

PREFIX is the prefix string.
COMPLETION is a plist of (:candidates :incomplete).")

(defun company-lsp--trigger-characters ()
  "Return a list of completion trigger characters specified by server."
  (let ((provider (lsp--capability "completionProvider")))
    (and provider (gethash "triggerCharacters" provider))))

(defun company-lsp--completion-prefix ()
  "Return the completion prefix.

Return value is compatible with the `prefix' command of a company backend.

Return nil if no completion should be triggered. Return a string
as the prefix to be completed, or a cons cell of (prefix . t) to bypass
`company-minimum-prefix-length' for trigger characters."
  (let ((trigger-chars (company-lsp--trigger-characters)))
    (if trigger-chars
        (let* ((max-trigger-len (apply 'max (mapcar (lambda (trigger-char)
                                                      (length trigger-char))
                                                    trigger-chars)))
               (trigger-regex (s-join "\\|" (mapcar #'regexp-quote trigger-chars)))
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
      (company-grab-symbol))))

(defun company-lsp--make-candidate (item)
  "Convert a CompletionItem JSON data to a string.

ITEM is a hashtable representing the CompletionItem interface.

The returned string has a lsp-completion-item property with the
value of ITEM."
  ;; The property has to be the same as added by `lsp--make-completion-item' so
  ;; that `lsp--annotate' can use it.
  (propertize (gethash "label" item) 'lsp-completion-item item))

(defun company-lsp--candidate-item (candidate)
  "Retrieve the CompletionItem hashtable associated with CANDIDATE.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (plist-get (text-properties-at 0 candidate) 'lsp-completion-item))

(defun company-lsp--resolve-candidate (candidate &rest props)
  "Resolve a completion candidate to fill some properties.

CANDIDATE is a string returned by `company-lsp--make-candidate'.
PROPS are strings of property names of CompletionItem hashtable
to be resolved.

The completionItem/resolve request will only be sent to the
server if the candidate has not been resolved before, and at lest
one of the PROPS of the CompletionItem is missing.

Returns CANDIDATE with the resolved CompletionItem."
  (unless (plist-get (text-properties-at 0 candidate) 'company-lsp-resolved)
    (let ((item (company-lsp--candidate-item candidate)))
      (when (seq-some (lambda (prop)
                        (null (gethash prop item)))
                      props)
        (let ((resolved-item (lsp--resolve-completion item))
              (len (length candidate)))
          (put-text-property 0 len
                             'lsp-completion-item resolved-item
                             candidate)
          (put-text-property 0 len
                             'company-lsp-resolved t
                             candidate)))))
  candidate)

(defun company-lsp--rust-completion-snippet (item)
  "Function providing snippet with the rust language.
It parses the function's signature in ITEM (a CompletionItem)
to expand its arguments."
  (-when-let* ((kind (gethash "kind" item))
               (is-function (= kind 3)))
    (let* ((detail (gethash "detail" item))
           (snippet (when (and detail (s-matches? "^\\(pub \\)?\\(unsafe \\)?fn " detail))
                      (-some--> (substring detail (1+ (s-index-of "(" detail)) (s-index-of ")" detail))
                                (replace-regexp-in-string "^[^,]*self\\(, \\)?" "" it)
                                (and (not (s-blank-str? it)) it)
                                (s-split ", " it)
                                (mapconcat (lambda (x) (format "${%s}" x)) it ", ")))))
      (concat "(" (or snippet "$1") ")$0"))))

(defun company-lsp--fallback-snippet (item)
  "Return the fallback snippet to expand for ITEM.

It looks for function corresponding to the language in
`company-lsp--snippet-functions'.

ITEM is a hashtable of the CompletionItem message.

Return a string of the snippet to expand, or nil if no snippet is available."
  (-when-let* ((language-id-fn (lsp--client-language-id (lsp--workspace-client lsp--cur-workspace)))
               (language-id (funcall language-id-fn (current-buffer)))
               (fn-cons (assoc language-id company-lsp--snippet-functions))
               (fn (cdr fn-cons)))
    (funcall fn item)))

(defun company-lsp--post-completion (candidate)
  "Replace a CompletionItem's label with its insertText. Apply text edits.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (let* ((resolved-candidate (company-lsp--resolve-candidate candidate
                                                             "insertText"
                                                             "textEdit"
                                                             "additionalTextEdits"))
         (item (company-lsp--candidate-item resolved-candidate))
         (label (gethash "label" item))
         (start (- (point) (length label)))
         (insert-text (gethash "insertText" item))
         ;; 1 = plaintext, 2 = snippet
         (insert-text-format (gethash "insertTextFormat" item))
         (text-edit (gethash "textEdit" item))
         (additional-text-edits (gethash "additionalTextEdits" item)))
    (cond
     (text-edit (lsp--apply-text-edit text-edit))
     ((and insert-text (not (eq insert-text-format 2)))
      (cl-assert (string-equal (buffer-substring-no-properties start (point)) label))
      (goto-char start)
      (delete-char (length label))
      (insert insert-text)))
    (when additional-text-edits
      (lsp--apply-text-edits additional-text-edits))
    (when (and company-lsp-enable-snippet
               (fboundp 'yas-expand-snippet))
      (if (and insert-text (eq insert-text-format 2))
          (yas-expand-snippet insert-text start (point))
        (-when-let (fallback-snippet (company-lsp--fallback-snippet item))
          (yas-expand-snippet fallback-snippet))))))

(defun company-lsp--on-completion (response prefix callback)
  "Give the server RESPONSE to company's CALLBACK.

PREFIX is a stirng of the prefix when the completion is requested."
  (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
         (items (cond ((hash-table-p response) (gethash "items" response))
                      ((sequencep response) response)))
         (candidates (mapcar #'company-lsp--make-candidate
                             (lsp--sort-completions items))))
    (when (null company-lsp--completion-cache)
      (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache)
      (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache))
    (when (eq company-lsp-cache-candidates 'auto)
      ;; Only cache candidates on auto mode. If it's t company caches the
      ;; candidates for us.
      (setq company-lsp--completion-cache
            (cons (cons prefix `(:incomplete ,incomplete :candidates ,candidates))
                  company-lsp--completion-cache)))
    (funcall callback candidates)))

(defun company-lsp--cleanup-cache (_)
  "Clean up completion cache and company hooks."
  (setq company-lsp--completion-cache nil)
  (remove-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache)
  (remove-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache))

(defun company-lsp--cache-get (prefix)
  "Get the cached completion for PREFIX.

Return a plist of (:incomplete :candidates) if cache for PREFIX
exists. Otherwise return nil."
  (cdr (assoc prefix company-lsp--completion-cache)))

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
           #'(lambda (callback)
               (let ((cache (company-lsp--cache-get arg)))
                 (if cache
                     (funcall callback (plist-get cache :candidates))
                   (lsp--send-changes lsp--cur-workspace)
                   (let ((req (lsp--make-request "textDocument/completion"
                                                 (lsp--text-document-position-params))))
                     (if company-lsp-async
                         (lsp--send-request-async req
                                                  (lambda (resp)
                                                    (company-lsp--on-completion resp arg callback)))
                       (company-lsp--on-completion (lsp--send-request req)
                                                   arg
                                                   callback))))))))
    (sorted t)
    (no-cache (if (eq company-lsp-cache-candidates 'auto)
                  (let ((cache (company-lsp--cache-get arg)))
                    (and cache (plist-get cache :incomplete)))
                (not company-lsp-cache-candidates)))
    (annotation (lsp--annotate arg))
    (match (length arg))
    (post-completion (company-lsp--post-completion arg))))

(defun company-lsp--client-capabilities ()
  "Return the extra client capabilities supported by company-lsp."
  (when company-lsp-enable-snippet
    '(:textDocument (:completion (:completionItem (:snippetSupport t))))))

(add-hook 'lsp-before-initialize-hook
          (lambda ()
            (lsp-register-client-capabilities 'company-lsp #'company-lsp--client-capabilities)))

(provide 'company-lsp)
;;; company-lsp.el ends here
