;;; lismp.el --- functions to manipulate items in lists

;; Copyright (C) 1994 Simon Marshall.

;; Author: Simon Marshall <Simon.Marshall@bogus.example.com>
;; Keywords: lists
;; Version: 1.02

;; LCD Archive Entry:
;; lismp|Simon Marshall|Simon.Marshall@bogus.example.com|
;; Functions to manipulate (add, remove, replace) items to/from/in lists.|
;; 09-May-1994|1.02|~/misc/lismp.el.Z|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;; 
;; Fed up with not having any functions to manipulate lists in your ~/.emacs or
;; package?  This package provides functions to add items to, remove items
;; from, and replace items in, lists.  Provided are the functions `add-item',
;; `delete-item' and `replace-item'.
;;
;; The function `add-item' can prepend or append items to a list:
;;
;; (let ((fubar '(b c d e)))
;;   (add-item 'fubar 'a)
;;   fubar)
;;      => (a b c d e)
;;
;; (let ((fubar '(b c d e)))
;;   (add-item 'fubar 'f nil nil t)
;;   fubar)
;;      => (b c d e f)
;;
;; But will not add to the list if the item is already on the list:
;;
;; (let ((fubar '(b c d e)))
;;   (add-item 'fubar 'd)
;;   fubar)
;;      => (b c d e)
;;
;; You can specify the predicate and key used to determine whether the item is
;; already on the list:
;;
;; (let ((auto-mode-alist '(("\\.el\\'" . emacs-lisp-mode) 
;;                          ("\\.c\\'" . c-mode))))
;;   (add-item 'auto-mode-alist '("\\.html\\'" . html-mode)
;;             'assoc "\\.html\\'")
;;   auto-mode-alist)
;;      => (("\\.html\\'" . html-mode) ("\\.el\\'" . emacs-lisp-mode)
;;          ("\\.c\\'" . c-mode))
;;
;; (let ((auto-mode-alist '(("\\.c\\'" . c-mode) ("\\.html\\'" . html-mode))))
;;   (add-item 'auto-mode-alist '("\\.html\\'" . html-mode)
;;             'assoc "\\.html\\'")
;;   auto-mode-alist)
;;      => (("\\.c\\'" . c-mode) ("\\.html\\'" . html-mode))
;;
;; The return value tells you whether the item was added:
;;
;; (add-item 'completion-ignored-extensions "%")
;;      => ("%" ".o" ".elc" "~" ".bin" ".lbin" ".fasl" ".dvi" ".toc" ".log"
;;          ".aux" ".a" ".ln" ".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".fmt")
;; (add-item 'completion-ignored-extensions "%")
;;      => nil
;;
;; The function `delete-item' deletes items from a list:
;;
;; (let ((fubar '(a b c (b) ((b) foo) ((b) bar) e b)))
;;   (delete-item 'fubar 'b)
;;   fubar)
;;      => (a c (b) ((b) foo) ((b) bar) e)
;;
;; You can specify the predicate and key used to determine those item(s) that
;; are to be deleted from the list:
;;
;; (let ((fubar '(a b c (b) ((b) foo) ((b) bar) e b)))
;;   (delete-item 'fubar 'b 'assoc)
;;   fubar)
;;      => (a b c ((b) foo) ((b) bar) e b)
;;
;; (let ((fubar '(a b c (b) ((b) foo) ((b) bar) e b)))
;;   (delete-item 'fubar nil 'assoc '(b))
;;   fubar)
;;      => (a b c (b) e b)
;;
;; Note that both ((b) foo) and ((b) bar) were deleted from `fubar', since the
;; `assoc' of the list (b) in `fubar' returned both ((b) foo) and ((b) bar) as
;; items for deletion.  Note also that the final instance of `delete-item' is
;; the same as (delete-item 'fubar '(b) 'assoc)
;;
;; (let ((auto-mode-alist '(("\\.el\\'" . emacs-lisp-mode) 
;;                          ("\\.html\\'" . html-mode))))
;;   (delete-item 'auto-mode-alist nil 'assoc "\\.html\\'")
;;   auto-mode-alist)
;;      => (("\\.el\\'" . emacs-lisp-mode))
;;
;; The functions `add-item' and `delete-item' in combination:
;;
;; (delete-item 'auto-mode-alist "\\.pl\\'" 'assoc)         ; e.g., for Prolog.
;; (add-item 'auto-mode-alist '("\\.pl\\'" . perl-mode))
;;
;; The function `replace-item' changes each occurrence of a particular item:
;;
;; (let ((fubar '(a x c d e f)))
;;   (replace 'fubar 'x 'b))
;;      => (a b c d e f)
;;
;; (let ((fubar '(a b c (b) ((b) foo) ((b) bar) e b)))
;;   (replace 'fubar nil 'x 'assoc '(b)))
;;      => (a b c (b) x x e b)
;;
;; Note how each occurrence of an item matching (b) was replaced.
;;
;; (replace-item 'auto-mode-alist nil '("\\.pl\\'" . perl-mode)
;;               'assoc "\\.pl\\'"))
;;
;; Note that this is the same as
;; (replace-item 'auto-mode-alist "\\.pl\\'" '("\\.pl\\'" . perl-mode) 'assoc)

;; Installation:
;; 
;; To use, put in your package that uses these functions:
;;
;; (require 'lismp)
;;
;; or autoload in your ~/.emacs the specific functions you require:
;;
;; (autoload 'add-item "lismp"
;;   "Add to the value of ALIST the value ITEM if not already present.")
;; (autoload 'delete-item "lismp"
;;   "Delete from the value of ALIST the value ITEM.")
;; (autoload 'replace-item "lismp"
;;   "Replace in the value of ALIST each value OLD with NEW.")

;; Feedback:
;;
;; This is hand written software.  Use it at your own risk.
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;     - Simon Marshall (Simon.Marshall@bogus.example.com)
;; Don't forget the version number of the package.

(defun add-item (alist item &optional predicate key append)
  "Add to the value of ALIST the value ITEM if not already present.
Tests ALIST using function `member' unless optional PREDICATE is given.
Tests ALIST with ITEM unless optional KEY is given.
ITEM is added at the beginning of ALIST unless optional APPEND is non-nil.
Returns the new value of ALIST, or nil if the value of ALIST is unchanged.

ALIST and PREDICATE, if given, should be symbols.
PREDICATE, if given, should return non-nil if the element is in the list.
PREDICATE receives arguments ITEM (or KEY, if given) and the value of ALIST.

See also `delete-item' and `replace-item'."
  (let ((old (symbol-value alist)))
    (if (not (funcall (or predicate 'member) (or key item) old))
        (set alist (if append (nconc old (list item)) (cons item old))))))

(defun delete-item (alist item &optional predicate key)
  "Delete from the value of ALIST the value ITEM.
Deletes using function `delete' unless optional PREDICATE is given.
Deletes ITEM unless optional KEY is given.

ALIST and PREDICATE, if given, should be symbols.
PREDICATE, if given, should return an element that should be deleted.
PREDICATE receives arguments ITEM (or KEY, if given) and the value of ALIST.

See also `add-item' and `replace-item'."
  (if (not predicate)
      (set alist (delete (or key item) (symbol-value alist)))
    (let (del)
      (while (setq del (funcall predicate (or key item) (symbol-value alist)))
        (set alist (delete del (symbol-value alist)))))))

(defun replace-item (alist old new &optional predicate key)
  "Replace in the value of ALIST each value OLD with NEW.
Tests ALIST using function `member' unless optional PREDICATE is given.
Tests ALIST with OLD unless optional KEY is given.

ALIST and PREDICATE, if given, should be symbols.
PREDICATE, if given, should return non-nil if the element should be replaced.
PREDICATE receives arguments OLD (or KEY, if given) and a list comprising an
item in value of ALIST.

See also `add-item' and `delete-item'."
  (let ((predicate (or predicate 'member)) (key (or key old)))
    (set alist (mapcar (function (lambda (item)
                         (if (funcall predicate key (list item)) new item)))
                       (symbol-value alist)))))

;;; Functions for emacs-18

(or (fboundp 'delete)
    (defun delete (elt list)
      "Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT there is no way to remove it by side effect;
therefore, write `(setq foo (delete element foo))'
to be sure of changing the value of `foo'."
      (let ((list list))
        (while list
          (while (equal elt (nth 1 list))
            (setcdr list (nthcdr 2 list)))
          (setq list (cdr list))))
      (if (equal elt (car list)) (cdr list) list)))

(or (fboundp 'member)
    (defun member (elt list)
      "Return non-nil if ELT is an element of LIST.
Comparison is done with `equal'.
The value is actually the tail of LIST whose car is ELT."
      (while (and list (not (equal elt (car list))))
        (setq list (cdr list)))
      list))

(provide 'lismp)
(provide 'add-item)

;;; lismp.el ends here





