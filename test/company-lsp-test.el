;;; company-lsp-test.el --- Tests for company-lsp.el -*-lexical-binding:t-*-

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Define butterbup test-suites to test company-lsp. This test suite should pass
;; for all Emacs versions defined in the .travis.yml file in the project
;; directory root.

;;; Code:

(require 'company-lsp)

(defun company-lsp--test-snippet-function (item)
  (gethash "detail" item))

(defun ensure-candidate-item (candidate)
  (if (company-lsp--candidate-item candidate)
      candidate
    (let ((completion-item (make-hash-table :test 'equal)))
      (puthash "label" (substring-no-properties candidate) completion-item)
      (company-lsp--make-candidate completion-item ""))))

(defun ensure-candidates-items (candidates)
  (mapcar #'ensure-candidate-item candidates))

(describe "company-lsp--fallback-snippet"
  :var (company-lsp--snippet-functions item language-id lsp--cur-workspace)
  (before-each
    (setq company-lsp--snippet-functions
          '(("test" . company-lsp--test-snippet-function)))
    (setq item (make-hash-table :test 'equal))
    (puthash "detail" "some-detail" item)
    (puthash "label" "some-label" item)
    (spy-on 'lsp-buffer-language :and-call-fake (lambda () language-id)))

  (it "Calls the snippet function for test language ID"
    (setq language-id "test")
    (expect (company-lsp--fallback-snippet item) :to-equal "some-detail")
    )

  (it "Returns nil if snippet function doesn't exist for language ID"
    (setq language-id "foo")
    (expect (company-lsp--fallback-snippet item) :to-be nil)))

(describe "company-lsp--rust-completion-snippet"
  (describe "With functions"
    (it "Replaces 1 arguments with placeholders"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "fn foo(arg1) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "(${arg1})$0")))

    (it "Replaces 2 arguments with placeholders"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "fn foo(arg1, arg2) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "(${arg1}, ${arg2})$0")))

    (it "Can handle 0 parameter"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "fn foo() -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "($1)$0")))

    (it "Removes self"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "fn foo(*self, arg1, arg2) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "(${arg1}, ${arg2})$0")))

    (it "Remove mut self"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "fn foo(&mut self, arg1, arg2) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "(${arg1}, ${arg2})$0")))

    (it "Remove self with lifetime"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "fn foo(&'a self, arg1, arg2) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "(${arg1}, ${arg2})$0")))

    (it "Can handle self as the only parameter"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "fn foo(*self) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "($1)$0")))

    (it "Can handle pub functions"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "pub fn foo(arg1) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "(${arg1})$0")))

    (it "Can handle unsafe functions"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "unsafe fn foo(arg1) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "(${arg1})$0")))

    (it "Can handle pub unsafe functions"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 3 item)
        (puthash "detail" "pub unsafe fn foo(arg1) -> bar" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-equal "(${arg1})$0")))
    )
  (describe "With non-functions"
    (it "Should return nil"
      (let ((item (make-hash-table :test 'equal)))
        (puthash "kind" 2 item)
        (puthash "detail" "(1, 2)" item)
        (expect (company-lsp--rust-completion-snippet item)
                :to-be nil)))))

(describe "Getting cache"
  (it "Should return the cache item if prefix matches"
    (let ((company-lsp--completion-cache nil)
          (cache-item (company-lsp--cache-item-new (ensure-candidates-items '("foo" "bar"))
                                                   nil)))
      (company-lsp--cache-put "prefix" cache-item)
      (expect (company-lsp--cache-get "prefix")
              :to-equal cache-item)))

  (it "Should return the cache item of sub-prefix if it's complete"
    (let ((cache-item (company-lsp--cache-item-new (ensure-candidates-items '("prefix1234" "prefix12345"))
                                                   nil)))
      (setq company-lsp--completion-cache nil)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal nil)
      (company-lsp--cache-put "" cache-item)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal cache-item)

      (setq company-lsp--completion-cache nil)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal nil)
      (company-lsp--cache-put "prefix" cache-item)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal cache-item)

      (setq company-lsp--completion-cache nil)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal nil)
      (company-lsp--cache-put "prefix123" cache-item)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal cache-item)))

  (it "Should filter cache items of sub-prefix"
    (let ((cache-item (company-lsp--cache-item-new
                       (ensure-candidates-items '("prefix" "prefix123" "prefix1234" "prefix12345"))
                       nil)))
      (company-lsp--cache-put "prefix" cache-item)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal
              (company-lsp--cache-item-new '("prefix1234" "prefix12345") nil))))

  (it "Should not return the cache item of sub-prefix if it's incomplete"
    (let ((company-lsp--completion-cache nil)
          (cache-item (company-lsp--cache-item-new (ensure-candidates-items '("foo" "bar"))
                                                   t)))
      (company-lsp--cache-put "prefix" cache-item)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal nil))))

(describe "company-lsp--to-yasnippet-snippet"
  (it "Should return the same simple snippet"
    (expect (company-lsp--to-yasnippet-snippet "foo")
            :to-equal "foo"))

  (it "Should return the same snippet with fields"
    (expect (company-lsp--to-yasnippet-snippet "foo(${1:bar}, ${2:baz})")
            :to-equal "foo(${1:bar}, ${2:baz})"))

  (it "Should escape opening brackets that are not field start"
    (expect (company-lsp--to-yasnippet-snippet "foo(${1:{{${2:}{bar})")
            :to-equal "foo(${1:\\{\\{${2:}\\{bar})")))

(describe "company-lsp--get-config"
  (it "Should return server config if present"
    (expect (company-lsp--get-config '((foo . nil) (bar . "bar") (t . "none"))
                                     'foo)
            :to-equal nil)
    (expect (company-lsp--get-config '((foo . nil) (bar . "bar") (t . "none"))
                                     'bar)
            :to-equal "bar"))

  (it "Should return fallback config if server is not present"
    (expect (company-lsp--get-config '((foo . "foo") (t . "none"))
                                     'bar)
            :to-equal "none"))

  (it "Should return nil if server is not present and fallback is not present"
    (expect (company-lsp--get-config '((foo . "foo"))
                                     'bar)
            :to-equal nil))

  (it "Should return the config if it's not a list"
    (expect (company-lsp--get-config "bar" 'foo) :to-equal "bar")
    (expect (company-lsp--get-config t 'foo) :to-equal t)
    (expect (company-lsp--get-config nil 'foo) :to-equal nil)))

(describe "company-lsp--filter-candidates"
  (describe "with prefix match"
    (before-each
      (setq company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix))
    (it "Should filter using filterText if available"
      (defun make-candidate (label filterText)
        (let ((item (make-hash-table :test 'equal)))
          (puthash "label" label item)
          (puthash "filterText" filterText item)
          (company-lsp--make-candidate item "")))

      (let ((candidates (list (make-candidate " 1" "foo")
                              (make-candidate " 2" "bar"))))
        (expect (company-lsp--filter-candidates candidates "f")
                :to-equal '(" 1"))))

    (it "Should filter using label if filterText is absent"
      (let ((candidates (ensure-candidates-items '("foo" "bar"))))
        (expect (company-lsp--filter-candidates candidates "f")
                :to-equal '("foo")))))

  (describe "with flex match"
    (before-each
      (setq company-lsp-match-candidate-predicate #'company-lsp-match-candidate-flex))

    (it "Should filter using filterText if available"
      (defun make-candidate (label filterText)
        (let ((item (make-hash-table :test 'equal)))
          (puthash "label" label item)
          (puthash "filterText" filterText item)
          (company-lsp--make-candidate item "")))

      (let ((candidates (list (make-candidate " 1" "zab")
                              (make-candidate " 2" "zabc"))))
        (expect (company-lsp--filter-candidates candidates "abc")
                :to-equal '(" 2"))))

    (it "Should filter using label if filterText is absent"
      (let ((candidates (ensure-candidates-items
                         '("ab" "zabc" "abcz" "zabcz" "zazbzcz" "cbabc"
                           "ABC" "abc" "acb" "cab" "cba"))))
        (expect (mapcar #'substring-no-properties
                        (company-lsp--filter-candidates candidates "abc"))
                :to-equal '("abc" "abcz" "ABC" "zabc" "zabcz" "cbabc" "zazbzcz"))))))

(describe "company-lsp--compute-flex-match"
  (describe "with full match"
    (it "matches empty string"
      (expect (cdr (company-lsp--compute-flex-match "abc" "" t))
              :to-equal '((0 . 0))))

    (it "matches exactly same string"
      (expect (cdr (company-lsp--compute-flex-match "abc" "abc" t))
              :to-equal '((0 . 3))))

    (it "matches suffix"
      (expect (cdr (company-lsp--compute-flex-match "zabc" "abc" t))
              :to-equal '((1 . 4))))

    (it "matches prefix"
      (expect (cdr (company-lsp--compute-flex-match "abcz" "abc" t))
              :to-equal '((0 . 3))))

    (it "matches consecutive substring"
      (expect (cdr (company-lsp--compute-flex-match "zabcz" "abc" t))
              :to-equal '((1 . 4))))

    (it "matches flexly"
      (expect (cdr (company-lsp--compute-flex-match "zazbzcz" "abc" t))
              :to-equal '((1 . 2) (3 . 4) (5 . 6))))

    (it "is case-insensitive"
      (expect (cdr (company-lsp--compute-flex-match "zAzbzcz" "abC" t))
              :to-equal '((1 . 2) (3 . 4) (5 . 6))))

    (it "does not partial match"
      (expect (cdr (company-lsp--compute-flex-match "ab" "abc" t))
              :to-equal nil)
      (expect (cdr (company-lsp--compute-flex-match "a" "abc" t))
              :to-equal nil)
      (expect (cdr (company-lsp--compute-flex-match "bc" "abc" t))
              :to-equal nil))

    (it "does not match substrings out of order"
      (expect (cdr (company-lsp--compute-flex-match "cbabc" "abc" t))
              :to-equal '((2 . 5)))
      (expect (cdr (company-lsp--compute-flex-match "cba" "abc" t))
              :to-equal nil)))

  (describe "does partial match of sub-prefix"
    (it "does partial match"
      (expect (cdr (company-lsp--compute-flex-match "abc" "" nil))
              :to-equal '((0 . 0)))
      (expect (cdr (company-lsp--compute-flex-match "abc" "abc" nil))
              :to-equal '((0 . 3)))
      (expect (cdr (company-lsp--compute-flex-match "zazb" "abc" nil))
              :to-equal '((1 . 2) (3 . 4)))
      (expect (cdr (company-lsp--compute-flex-match "zbza" "abc" nil))
              :to-equal '((3 . 4)))
      (expect (cdr (company-lsp--compute-flex-match "bc" "abc" nil))
              :to-equal nil))))
