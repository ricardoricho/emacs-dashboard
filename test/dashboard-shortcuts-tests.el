;;; dashboard-shortcuts-tests.el --- Test section shortcut setup  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 emacs-dashboard maintainers

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ERT tests for section shortcut setup in dashboard.
;;

;;; Code:

(require 'ert)
(require 'dashboard)

(ert-deftest dashboard-insert-shortcut-test ()
  "Setting up a section shortcut binds cycling commands."
  (let ((dashboard-mode-map (make-sparse-keymap)))
    (with-temp-buffer
      (insert "Recent Files:\n")
      (dashboard-insert-shortcut 'recents "r" "Recent Files:"))
    (should (commandp (lookup-key dashboard-mode-map "r")))
    (should (commandp (lookup-key dashboard-mode-map "R")))))

(ert-deftest dashboard-insert-shortcut-navigation-test ()
  "Setting up a section shortcut preserves point and enables navigation."
  (let ((dashboard-mode-map (make-sparse-keymap)))
    (with-temp-buffer
      (insert "Recent Files:\n    ")
      (let ((first-item-position (point)))
        (widget-create 'item :tag "one" :button-prefix ""
                       :button-suffix "" :format "%[%t%]")
        (insert "\n    ")
        (widget-create 'item :tag "two" :button-prefix ""
                       :button-suffix "" :format "%[%t%]")
        (goto-char (point-min))
        (dashboard-insert-shortcut 'recents "r" "Recent Files:")
        (should (= (point) (point-min)))
        (call-interactively (lookup-key dashboard-mode-map "r"))
        (should (= (point) first-item-position))))))

;;; dashboard-shortcuts-tests.el ends here
