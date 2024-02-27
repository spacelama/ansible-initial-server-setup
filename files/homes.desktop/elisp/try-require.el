;; -*- emacs-lisp -*-
;;; try-require.el --- attempt to load a feature/library, failing silently

;; Author: Mark Triggs <mst@dishevelled.net>, Damien Elmes <resolve@repose.cx>
;; $Id: try-require.el,v 1.1 2004/03/20 10:46:00 tconnors Exp $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl)

(defun try-require (&rest args)
  "Attempt to load a library or module. Return true if all of the libraries
  given as arguments are successfully loaded"
  (cl-typecase (car args)
    (null t)
    (string (and (condition-case e
                     (load-library (car args))
                   (file-error () nil))
                 (apply #'try-require (cdr args))))
    (symbol (and (condition-case e
                     (require (car args))
                   (file-error () nil))
                 (apply #'try-require (cdr args))))))


(provide 'try-require)
;;; try-require.el ends here
