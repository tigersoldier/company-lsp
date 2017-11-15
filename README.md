[![MELPA](https://melpa.org/packages/company-lsp-badge.svg)](https://melpa.org/#/company-lsp)

# company-lsp
[Company] completion backend for [lsp-mode].

![Completion with snippet expansion.](https://user-images.githubusercontent.com/5273820/32362179-99550794-c067-11e7-9568-3c46fb31493c.gif)

It provides features that are not available by using `company-capf` + `lsp-mode`:

 * Support trigger characters. For example typing `.` will trigger completion
   for TypeScript.
 * Use completion item's `label` as completion labels and replace it with its
   `insertText` if available.
 * Fetch completion candidates asynchronously (Thanks @sebastiencs).
 * Apply text edits specified by completion candidates after completion.
 * Do not cache the completion candidates if they are incomplete.
 * Expand snippets on completion (requires [yasnippet]).

## Usage

Company-lsp is available on [MELPA]. To install it, first [setup
MELPA][setup-melpa], then `M-x package-install <RET> company-lsp`.

After installing company-lsp, simply add `company-lsp` to `company-backends`:

```elisp
(require 'company-lsp)
(push 'company-lsp company-backends)
```

## Customization

 * `company-lsp-cache-candidates`: Can be set to `'auto`, `t`, or `nil`. When
    set to `'auto`, candidates will not be cached if the server returns
    incomplete completion list. When set to `t`, company caches the completion
    candidates and filters the candidates as completion progresses. If set to
    `nil`, each incremental completion triggers a completion request to the
    language server.
 * `company-lsp-async`: When set to non-nil, fetch completion candidates
    asynchronously.
 * `company-lsp-enable-snippet`: Set it to non-nil if you want to enable snippet
    expansion on completion. Set it to nil to disable this feature.
    
## Defining completion snippet for a certain language

If the language server for that language doesn't support returning snippets, you
can customize the variable `company-lsp--snippet-functions` do define snippets
for candidates. `company-lsp--snippet-functions` is an alist of `(LANGUAGE-ID .
SNIPPET-FUNCTION)`.

`LANGUAGE-ID` is the language ID defined by the lsp client. Currently there is
no good way knowing it other than guessing or reading the code of the lsp client
for the language. For example if you use [lsp-rust], it's defined as following
in `lsp-rust.el`:

```elisp
(lsp-define-stdio-client lsp-rust "rust" #'lsp-rust--get-root nil
			 :command-fn #'lsp-rust--rls-command
			 :initialize #'lsp-rust--initialize-client)
```

The language ID is the second parameter, `"rust"`.

`SNIPPET-FUNCTION` is a function that transforms a hash table representing the
[CompletionItem] message to a snippet string or nil. Below is an example on how
to extact a snippet for Rust function parameters:

```elisp
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
                                (s-split ", " it)
                                (mapconcat (lambda (x) (format "${%s}" x)) it ", ")))))
      (concat "(" (or snippet "$1") ")$0"))))
```

[company]: http://company-mode.github.io/
[CompletionItem]: https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textDocument_completion
[lsp-mode]: https://github.com/emacs-lsp/lsp-mode
[lsp-rust]: https://github.com/emacs-lsp/lsp-rust
[melpa]: https://melpa.org
[setup-melpa]: https://melpa.org/#/getting-started
[yasnippet]: https://github.com/joaotavora/yasnippet
