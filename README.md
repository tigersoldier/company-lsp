[![MELPA](https://melpa.org/packages/company-lsp-badge.svg)](https://melpa.org/#/company-lsp)

# company-lsp
[Company] completion backend for [lsp-mode].

It provides features that are not available by using `company-capf` + `lsp-mode`:

 * Support trigger characters. For example typing `.` will trigger completion
   for TypeScript.
 * Use completion item's `label` as completion labels and replace it with its
   `insertText` if available.
 * Fetch completion candidates asynchronously (Thanks @sebastiencs).
 * Apply text edits specified by completion candidates after completion.
 * Do not cache the completion candidates if they are incomplete.

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
 * `company-lsp-async`: When set to non-til, fetch completion candidates
    asynchronously.

[company]: http://company-mode.github.io/
[lsp-mode]: https://github.com/emacs-lsp/lsp-mode
[melpa]: https://melpa.org
[setup-melpa]: https://melpa.org/#/getting-started
