;;; company-lsp.el --- Company completion backend for lsp-mode.  -*- lexical-binding: t -*-

;; Version: 2.0.1
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.4") (company "0.9.0") (s "1.2.0") (dash "2.11.0"))
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

When set to 'auto, company-lsp caches the completion. It sends
incremental completion requests to the server if and only if the
cached results are incomplete. The candidate list may not be
sorted or filtered as the server would for cached completion
results.

When set to t, company-mode caches the completion. It won't send
incremental completion requests to the server.

When set to nil, results are not cached at all. The candidates
are always sorted and filtered by the server. Use this option if
the server handles caching for incremental completion or
sorting/matching provided by the server is critical."
  :type '(choice (const :tag "Respect server response" auto)
                 (const :tag "Always cache" t)
                 (const :tag "Never cache" nil))
  :group 'company-lsp)

(defcustom company-lsp-async t
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

(defcustom company-lsp-enable-recompletion nil
  "Whether or not to re-trigger completion for trigger characters.

If set to non-nil, when company-lsp finishes completion, it checks if
the current point is before any completion trigger characters. If yes,
it re-triggers another completion request.

This is useful in cases such as 'std' is completed as 'std::' in C++."
  :type 'boolean
  :group 'company-lsp)

(declare-function yas-expand-snippet "ext:yasnippet.el")

(defvar company-lsp--snippet-functions '(("rust" . company-lsp--rust-completion-snippet))
  "Alist of functions to insert our snippets for each language.")

(defvar-local company-lsp--completion-cache nil
  "Cached completion. It's an alist of (prefix . completion).

PREFIX is the prefix string.
COMPLETION is a cache-item created by `company-lsp--cache-item-new'.")

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

(defun company-lsp--make-candidate (item prefix)
  "Convert a CompletionItem JSON data to a string.

ITEM is a hashtable representing the CompletionItem interface.
PREFIX is the currently active prefix.

The returned string has a lsp-completion-item property with the
value of ITEM."
  ;; The property has to be the same as added by `lsp--make-completion-item' so
  ;; that `lsp--annotate' can use it.
  (propertize (gethash "label" item) 'lsp-completion-item item 'lsp-completion-prefix prefix))

(defun company-lsp--candidate-item (candidate)
  "Retrieve the CompletionItem hashtable associated with CANDIDATE.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (plist-get (text-properties-at 0 candidate) 'lsp-completion-item))

(defun company-lsp--candidate-prefix (candidate)
  "Retrieves the prefix that was active during creation of the candidate.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (plist-get (text-properties-at 0 candidate) 'lsp-completion-prefix))

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

(defun company-lsp--looking-back-trigger-characters-p ()
  "Return non-nil if text before point matches any of the trigger characters."
  (let ((trigger-chars (company-lsp--trigger-characters)))
    (cl-some (lambda (trigger-char)
               (equal (buffer-substring-no-properties (- (point) (length trigger-char)) (point))
                      trigger-char))
             trigger-chars)))

(defun company-lsp--post-completion (candidate)
  "Replace a CompletionItem's label with its insertText. Apply text edits.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (let* ((resolved-candidate (company-lsp--resolve-candidate candidate
                                                             "insertText"
                                                             "textEdit"
                                                             "additionalTextEdits"))
         (item (company-lsp--candidate-item resolved-candidate))
         (prefix (company-lsp--candidate-prefix candidate))
         (label (gethash "label" item))
         (start (- (point) (length label)))
         (insert-text (gethash "insertText" item))
         ;; 1 = plaintext, 2 = snippet
         (insert-text-format (gethash "insertTextFormat" item))
         (text-edit (gethash "textEdit" item))
         (additional-text-edits (gethash "additionalTextEdits" item)))
    (cond
     (text-edit
      (setq insert-text (gethash "newText" text-edit))
      (delete-region (- (point) (length candidate)) (point))
      (insert prefix)
      (let* ((range (gethash "range" text-edit))
             (start-point (lsp--position-to-point (gethash "start" range)))
             (new-text-length (length insert-text)))
        (lsp--apply-text-edit text-edit)
        (goto-char (+ start-point new-text-length))))
     ((and insert-text (not (eq insert-text-format 2)))
      (cl-assert (string-equal (buffer-substring-no-properties start (point)) label))
      (goto-char start)
      (delete-char (length label))
      (insert insert-text)))

    (let ((start-marker (set-marker (make-marker) start)))
      (when additional-text-edits
        (lsp--apply-text-edits additional-text-edits))
      (when (and company-lsp-enable-snippet
                 (fboundp 'yas-expand-snippet))
        (if (and insert-text (eq insert-text-format 2))
            (yas-expand-snippet insert-text (marker-position start-marker) (point))
          (-when-let (fallback-snippet (company-lsp--fallback-snippet item))
            (yas-expand-snippet fallback-snippet))))
      (set-marker start-marker nil))
    ;; Here we set this-command to a `self-insert-command'
    ;; so that company may retrigger idle completion after the snippet expansion
    ;; (~`company-post-command').
    ;; This is a bit of a hack and maybe that will change in the future.
    ;; This is useful for example when the completed candidate is a namespace
    ;; and the annotation text (inserted snippet) is the scope operator.
    ;;
    ;; std| -> std::   (=> idle completion desired here)
    ;;         stderr
    ;;         ...
    ;;
    ;; See https://github.com/company-mode/company-mode/issues/143
    (when (and company-lsp-enable-recompletion
               (company-lsp--looking-back-trigger-characters-p))
      (setq this-command 'self-insert-command))))

(defun company-lsp--on-completion (response prefix)
  "Handle completion RESPONSE.

PREFIX is a string of the prefix when the completion is requested.

Return a list of strings as the completion candidates."
  (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
         (items (cond ((hash-table-p response) (gethash "items" response))
                      ((sequencep response) response)))
         (candidates (mapcar (lambda (item)
                               (company-lsp--make-candidate item prefix))
                             (lsp--sort-completions items))))
    (when (null company-lsp--completion-cache)
      (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache)
      (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache))
    (when (eq company-lsp-cache-candidates 'auto)
      ;; Only cache candidates on auto mode. If it's t company caches the
      ;; candidates for us.
      (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
    candidates))

(defun company-lsp--cleanup-cache (_)
  "Clean up completion cache and company hooks."
  (setq company-lsp--completion-cache nil)
  (remove-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache)
  (remove-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache))

(defun company-lsp--cache-put (prefix candidates)
  "Set cache for PREFIX to be CANDIDATES.

CANDIDATES is a cache item created by `company-lsp--cache-item-new'."
  (setq company-lsp--completion-cache
        (cons (cons prefix candidates)
              company-lsp--completion-cache)))

(defun company-lsp--cache-get (prefix)
  "Get the cached completion for PREFIX.

Return a cache item if cache for PREFIX exists. Otherwise return nil."
  (let ((cache (cdr (assoc prefix company-lsp--completion-cache)))
        (len (length prefix))
        previous-cache)
    (if cache
        cache
      (cl-dotimes (i len)
        (when (setq previous-cache
                    (cdr (assoc (substring prefix 0 (- len i 1))
                                company-lsp--completion-cache)))
          (if (company-lsp--cache-item-incomplete-p previous-cache)
              (cl-return nil)
            ;; TODO: Allow customizing matching functions to support fuzzy matching.
            ;; Consider supporting company-flx out of box.
            (let* ((previous-candidates (company-lsp--cache-item-candidates previous-cache))
                   (new-candidates (all-completions prefix previous-candidates))
                   (new-cache (company-lsp--cache-item-new new-candidates nil)))
              (company-lsp--cache-put prefix new-cache)
              (cl-return new-cache))))))))

(defun company-lsp--cache-item-new (candidates incomplete)
  "Create a new cache item.

CANDIDATES: A list of strings. The completion candidates.
INCOMPLETE: t or nil. Whether the candidates are incomplete or not."
  (list :incomplete incomplete :candidates candidates))

(defun company-lsp--cache-item-incomplete-p (cache-item)
  "Determine whether a CACHE-ITEM is incomplete."
  (plist-get cache-item :incomplete))

(defun company-lsp--cache-item-candidates (cache-item)
  "Get candidates from a CACHE-ITEM."
  (plist-get cache-item :candidates))

(defun company-lsp--documentation (candidate)
  "Get the documentation from the item in the CANDIDATE.

The documentation can be either string or MarkupContent. This method
will return markdown string if it is MarkupContent, original string
otherwise. If the documentation is not present, it will return nil
which company can handle."
  (let* ((resolved-candidate (company-lsp--resolve-candidate candidate "documentation"))
         (item (company-lsp--candidate-item resolved-candidate))
         (documentation (gethash "documentation" item)))
    (if
        (hash-table-p documentation)  ;; If true, then the documentation is a MarkupContent. String otherwise.
        (gethash "value" documentation)
      documentation)))

(defun company-lsp--candidates-sync (prefix)
  "Get completion candidates synchronously.

PREFIX is the prefix string for completion.

Return a list of strings as completion candidates."
  (let ((req (lsp--make-request "textDocument/completion"
                                (lsp--text-document-position-params))))
    (company-lsp--on-completion (lsp--send-request req) prefix)))

(defun company-lsp--candidates-async (prefix callback)
  "Get completion candidates asynchronously.

PREFIX is the prefix string for completion.
CALLBACK is a function that takes a list of strings as completion candidates."
  (let ((req (lsp--make-request "textDocument/completion"
                                (lsp--text-document-position-params))))
    (lsp--send-request-async req
                             (lambda (resp)
                               (funcall callback (company-lsp--on-completion resp prefix))))))

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
     ;; If the completion items in the response have textEdit action populated,
     ;; we'll apply them in `company-lsp--post-completion'. However, textEdit
     ;; actions only apply to the pre-completion content. We backup the current
     ;; prefix and restore it after company completion is done, so the content
     ;; is restored and textEdit actions can be applied.
     (or (company-lsp--cache-item-candidates (company-lsp--cache-get arg))
         (and company-lsp-async
              (cons :async (lambda (callback)
                             (company-lsp--candidates-async arg callback))))
         (company-lsp--candidates-sync arg)))
    (sorted t)
    (no-cache (not (eq company-lsp-cache-candidates t)))
    (annotation (lsp--annotate arg))
    (quickhelp-string (company-lsp--documentation arg))
    (doc-buffer (company-doc-buffer (company-lsp--documentation arg)))
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
