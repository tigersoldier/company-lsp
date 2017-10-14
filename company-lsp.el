;;; company-lsp.el --- Company completion backend for lsp-mode.

;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (lsp-mode "2.0") (company "0.9.0"))
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

(require 'company)
(require 'lsp-mode)

(defgroup company-lsp nil
  "Company completion backend for lsp-mode."
  :prefix "company-lsp-"
  :group 'tools)

(defcustom company-lsp-cache-candidates t
  "When set to non-nil, company caches the completion candidates
so company filters the candidates as completion progresses. If
set to `nil', each incremental completion triggers a completion
request to the language server."
  :type 'boolean
  :group 'company-lsp)

(defun company-lsp--trigger-characters ()
  (when-let (completionProvider (lsp--capability "completionProvider"))
    (gethash "triggerCharacters" completionProvider)))

(defun company-lsp--completion-prefix ()
  (if-let (trigger-chars (company-lsp--trigger-characters))
      (let ((max-trigger-len (apply 'max (mapcar (lambda (trigger-char) (length trigger-char))
                                          trigger-chars)))
            (trigger-regex (reduce (lambda (accum elem)
                                     (concat accum "|" (regexp-quote elem)))
                                   trigger-chars
                                   :initial-value (regexp-quote (car trigger-chars))
                                   :start 1)))
        (company-grab-symbol-cons trigger-regex max-trigger-len))
    (company-grab-symbol)))


(defun company-lsp--make-candidate (item)
  (propertize (gethash "label" item) 'lsp-completion-item item))

(defun lsp-company-mode--insert-completion-text (candidate)
  (let* ((item (plist-get (text-properties-at 0 candidate) 'lsp-completion-item))
         (label (gethash "label" item))
         (start (- (point) (length label)))
         (insert-text (gethash "insertText" item)))
    (when insert-text
      (assert (string-equal (buffer-substring-no-properties start (point)) label))
      (goto-char start)
      (delete-char (length label))
      (insert insert-text))))

;;;###autoload
(defun company-lsp (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'company-lsp))
    (prefix
     (and
             (bound-and-true-p lsp-mode)
             (lsp--capability "completionProvider")
             (or (company-lsp--completion-prefix) 'stop)))
    (candidates
     (cons :async
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
                                           (lsp--sort-completions items)))))))
    (sorted t)
    (no-cache (not company-lsp-cache-candidates))
    (annotation (lsp--annotate arg))
    (match (length arg))
    (post-completion (lsp-company-mode--insert-completion-text arg))
    ))

(provide 'company-lsp)

;;; company-lsp.el ends here
