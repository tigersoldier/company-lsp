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

(describe "company-lsp--fallback-snippet"
  :var (company-lsp--snippet-functions item language-id lsp--cur-workspace)
  (before-each
    (setq company-lsp--snippet-functions
          '(("test" . company-lsp--test-snippet-function)))
    (setq item (make-hash-table :test 'equal))
    (puthash "detail" "some-detail" item)
    (puthash "label" "some-label" item)
    (setq lsp--cur-workspace
          (make-lsp--workspace
           :client (make-lsp--client :language-id (lambda (_) language-id)))))

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
          (cache-item '(:incomplete nil ("foo" "bar"))))
      (company-lsp--cache-put "prefix" cache-item)
      (expect (company-lsp--cache-get "prefix")
              :to-equal cache-item)))

  (it "Should return the cache item of sub-prefix if it's complete"
    (let ((cache-item (company-lsp--cache-item-new '("prefix1234" "prefix12345") nil)))
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
                       '("prefix" "prefix123" "prefix1234" "prefix12345")
                       nil)))
      (company-lsp--cache-put "prefix" cache-item)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal
              (company-lsp--cache-item-new '("prefix1234" "prefix12345") nil))))

  (it "Should not return the cache item of sub-prefix if it's incomplete"
    (let ((company-lsp--completion-cache nil)
          (cache-item '(:incomplete t ("foo" "bar"))))
      (company-lsp--cache-put "prefix" cache-item)
      (expect (company-lsp--cache-get "prefix1234")
              :to-equal nil))))


