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

## Usage

Company-lsp is available on [MELPA]. To install it, first [setup
MELPA][setup-melpa], then `M-x package-install <RET> company-lsp`.

After installing company-lsp, simply add `company-lsp` to `company-backends`:

```elisp
(require 'company-lsp)
(push 'company-lsp company-backends)
```

## Customization

 * `company-lsp-cache-candidates`: When set to non-nil, company caches the
    completion candidates so company filters the candidates as completion
    progresses. If set to `nil', each incremental completion triggers a
    completion request to the language server.
 * `company-lsp-async`: When set to non-til, fetch completion candidates
    asynchronously.

[company]: http://company-mode.github.io/
[lsp-mode]: https://github.com/emacs-lsp/lsp-mode
[melpa]: https://melpa.org
[setup-melpa]: https://melpa.org/#/getting-started
