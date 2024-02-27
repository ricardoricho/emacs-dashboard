;;; dashboard-org-agenda.el --- A emacs-dashboard extension for org-agenda -*- lexical-binding: t -*-
;; Copyright (c) 2024 emacs-dashboard maintainers
;; This file is not part of GNU Emacs.
;;; License: GPLv3
;;
;;; Commentary:
;; Dependencies org-mode, org-agenda and dashboard

;;; Code:

;; Compiler pacifier
(declare-function dashboard-insert-section "ext:dashboard.el")
(declare-function dashboard-get-shortcut "ext:dashboard.el")
(declare-function org-agenda-format-item "ext:org-agenda.el")
(declare-function org-compile-prefix-format "ext:org-agenda.el")
(declare-function org-entry-get "ext:org.el")
(declare-function org-entry-is-done-p "ext:org.el")
(declare-function org-entry-is-todo-p "ext:org.el")
(declare-function org-get-category "ext:org.el")
(declare-function org-get-deadline-time "ext:org.el")
(declare-function org-get-heading "ext:org.el")
(declare-function org-get-priority "ext:org.el")
(declare-function org-get-scheduled-time "ext:org.el")
(declare-function org-get-tags "ext:org.el")
(declare-function org-get-todo-face "ext:org.el")
(declare-function org-get-todo-state "ext:org.el")
(declare-function org-in-archived-heading-p "ext:org.el")
(declare-function org-map-entries "ext:org.el")
(declare-function org-outline-level "ext:org.el")
(declare-function org-release-buffers "ext:org.el")
(declare-function org-time-string-to-time "ext:org.el")
(declare-function org-today "ext:org.el")

(defalias 'org-time-less-p 'time-less-p)
(defvar org-level-faces)
(defvar org-agenda-new-buffers)
(defvar org-agenda-prefix-format)
(defvar org-agenda-todo-keyword-format)
(defvar org-todo-keywords-1)

;; Customs
;; TODO: Rename when extracting to another package

(defcustom dashboard-week-agenda t
  "Show agenda weekly if its not nil."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-agenda-time-string-format "%Y-%m-%d"
  "Format time of agenda entries."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-match-agenda-entry nil
  "Match agenda to extra filter.
It is the MATCH attribute for `org-map-entries'"
  :type 'string
  :group 'dashboard)

(defcustom dashboard-agenda-release-buffers nil
  "If not nil use `org-release-buffers' after getting the entries."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-time
  "Function to filter `org-agenda' entries."
  :type '(choice
          (const :tag "No filter" dashboard-no-filter-agenda)
          (const :tag "Filter by time" dashboard-filter-agenda-by-time)
          (const :tag "Filter by todo" dashboard-filter-agenda-by-todo)
          (function :tag "Custom function"))
  :group 'dashboard)

(defcustom dashboard-agenda-sort-strategy nil
  "A list of strategies to sort the agenda.  If nil agenda is not sorted."
  :type '(repeat (choice (const priority-up) (const priority-down)
                         (const time-up) (const time-down)
                         (const todo-state-up) (const todo-state-down)))
  :group 'dashboard)

(defcustom dashboard-agenda-prefix-format " %i %-12:c %s "
  "Format for each entry in the agenda.
When the dashboard-agenda is created this format is inserted into
`org-agenda-prefix-format' as `dashboard-agenda' and compiled with
`org-compile-prefix-format' previous calling `dashboard-agenda-entry-format' for
each agenda entry."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-agenda-tags-format 'identity
  "Function to format the org agenda tags.
Any custom function would receives the tags from `org-get-tags'"
  :type '(choice
          (const :tag "Show tags" identity)
          (const :tag "Hide tags" ignore)
          (function :tag "Custom function"))
  :group 'dashboard)

(defun dashboard-agenda-entry-format ()
  "Format agenda entry to show it on dashboard.
Also,it set text properties for latter use in different extensions actions."
  (let* ((scheduled-time (org-get-scheduled-time (point)))
         (deadline-time (org-get-deadline-time (point)))
         (entry-timestamp (dashboard-agenda--entry-timestamp (point)))
         (entry-time (or scheduled-time deadline-time entry-timestamp))
         (item (org-agenda-format-item
                (dashboard-agenda--formatted-time)
                (dashboard-agenda--formatted-headline)
                (org-outline-level)
                (org-get-category)
                (dashboard-agenda--formatted-tags)))
         (todo-state (org-get-todo-state))
         (item-priority (org-get-priority (org-get-heading t t t t)))
         (todo-index (and todo-state
                          (length (member todo-state org-todo-keywords-1))))
         (entry-data (list 'dashboard-agenda-file (buffer-file-name)
                           'dashboard-agenda-loc (point)
                           'dashboard-agenda-priority item-priority
                           'dashboard-agenda-todo-index todo-index
                           'dashboard-agenda-time entry-time)))
    (add-text-properties 0 (length item) entry-data item)
    item))

(defun dashboard-agenda--entry-timestamp (point)
  "Get the timestamp from an entry at POINT."
  (when-let ((timestamp (org-entry-get point "TIMESTAMP")))
    (org-time-string-to-time timestamp)))

(defun dashboard-agenda--formatted-headline ()
  "Set agenda faces to `HEADLINE' when face text property is nil."
  (let* ((headline (org-get-heading t t t t))
         (todo (or (org-get-todo-state) ""))
         (org-level-face (nth (- (org-outline-level) 1) org-level-faces))
         (todo-state (format org-agenda-todo-keyword-format todo)))
    (dashboard-agenda--set-face org-level-face headline)
    (dashboard-agenda--set-face (org-get-todo-face todo) todo-state)
    (concat todo-state " " headline)))

(defun dashboard-agenda--set-face (face text)
  "Add FACE to TEXT but inherit height from `dashboard-items-face'.
If not height is found on FACE or `dashboard-items-face' use `default'."
  (let ((height (face-attribute 'dashboard-items-face :height nil 'default)))
    (add-face-text-property 0 (length text) `((:height ,height) ,face) nil text)))

(defun dashboard-agenda--formatted-time ()
  "Get the scheduled or dead time of an entry.  If no time is found return nil."
  (when-let ((time (or (org-get-scheduled-time (point)) (org-get-deadline-time (point))
                       (dashboard-agenda--entry-timestamp (point)))))
    (format-time-string dashboard-agenda-time-string-format time)))

(defun dashboard-agenda--formatted-tags ()
  "Apply `dashboard-agenda-tags-format' to org-element tags."
  (when dashboard-agenda-tags-format
    (funcall dashboard-agenda-tags-format (org-get-tags))))

(defun dashboard-due-date-for-agenda ()
  "Return due-date for agenda period."
  (if dashboard-week-agenda
      (time-add (current-time) (* 86400 8))
    (time-add (current-time) 86400)))

(defun dashboard-filter-agenda-by-time ()
  "Include entry if it has a scheduled-time or deadline-time in the future.
An entry is included if this function returns nil and excluded if returns a
point."
  (let ((scheduled-time (org-get-scheduled-time (point)))
        (deadline-time (org-get-deadline-time (point)))
        (entry-timestamp (dashboard-agenda--entry-timestamp (point)))
        (due-date (dashboard-due-date-for-agenda))
        (now (current-time)))
    (unless (and (not (org-entry-is-done-p))
                 (not (org-in-archived-heading-p))
                 (or (and scheduled-time
                          (org-time-less-p scheduled-time due-date))
                     (and deadline-time
                          (org-time-less-p deadline-time due-date))
                     (and entry-timestamp
                          (org-time-less-p now entry-timestamp)
                          (org-time-less-p entry-timestamp due-date))))
      (point))))

(defun dashboard-filter-agenda-by-todo ()
  "Include entry if it is todo and not done.
An entry is included if this function returns nil and excluded
if returns a point."
  (unless (and (org-entry-is-todo-p)
               (not (org-entry-is-done-p))
               (not (org-in-archived-heading-p)))
    (point)))

(defun dashboard-no-filter-agenda ()
  "No filter agenda entries."
  (when (org-entry-is-done-p) (point)))

(defun dashboard-get-agenda ()
  "Get agenda items for today or for a week from now."
  (if-let ((prefix-format (assoc 'dashboard-agenda org-agenda-prefix-format)))
      (setcdr prefix-format dashboard-agenda-prefix-format)
    (push (cons 'dashboard-agenda dashboard-agenda-prefix-format) org-agenda-prefix-format))
  (org-compile-prefix-format 'dashboard-agenda)
  (prog1 (org-map-entries 'dashboard-agenda-entry-format
                          dashboard-match-agenda-entry
                          'agenda
                          dashboard-filter-agenda-entry)
    (dashboard-agenda--release-buffers)))

(defun dashboard-agenda--release-buffers ()
  "Release agenda buffers buffers.
This is what `org-agenda-exit' do."
  (when dashboard-agenda-release-buffers
    (org-release-buffers org-agenda-new-buffers)
    (setq org-agenda-new-buffers nil)))

(defun dashboard-agenda--sorted-agenda ()
  "Return agenda sorted by time.

For now, it only works when dashboard-agenda has been filter by time and
dashboard-agenda-sort is not nil."
  (let ((agenda (dashboard-get-agenda))
        (sort-function (dashboard-agenda--sort-function)))
    (sort agenda sort-function)))

(defun dashboard-agenda--sort-function ()
  "Get the function use to sorted the agenda.

Depending on the list `dashboard-agenda-sorting-strategy' use this strategies to
build a predicate to compare each enty.
This is similar as `org-entries-lessp' but with a different aproach."
  (dashboard-agenda--build-sort-function dashboard-agenda-sort-strategy))

(defun dashboard-agenda--build-sort-function (strategies)
  "Build a predicate to sort the dashboard agenda.

If `STRATEGIES' is nil then sort using the nil predicate.  Look for the strategy
predicate, the attributes of the entry and compare entries.  If no predicate is
found for the strategy it uses nil predicate."
  (if (null strategies) (lambda (_dont _care) nil)
    (let ((predicate (dashboard-agenda--build-sort-function-predicate
                      (car strategies)))
          (attribute (dashboard-agenda--build-sort-function-attribute
                      (car strategies))))
      (if (null predicate) (lambda (_dont _care) nil)
        (lambda (entry1 entry2)
          (dashboard-agenda--compare-entries entry1 entry2 (cdr strategies)
                                             predicate attribute))))))

(defun dashboard-agenda--build-sort-function-predicate (strategy)
  "Return the predicate to compare two entryes depending on the `STRATEGY'."
  (pcase strategy
    ('priority-up '>)
    ('priority-down '<)
    ('time-up 'org-time-less-p)
    ('time-down (lambda (a b) (org-time-less-p b a)))
    ('todo-state-up '>)
    ('todo-state-down '<)))

(defun dashboard-agenda--build-sort-function-attribute (strategy)
  "Return the argument to compare two entries depending to the `STRATEGY'."
  (cond
   ((memq strategy '(priority-up priority-down)) 'dashboard-agenda-priority)
   ((memq strategy '(time-up time-down)) 'dashboard-agenda-time)
   ((memq strategy '(todo-state-up todo-state-down)) 'dashboard-agenda-todo-index)
   (t nil)))

(defun dashboard-agenda--compare-entries (entry1 entry2 strategies predicate attribute)
  "Compare `ENTRY1' and `ENTRY2' by `ATTRIBUTE' using `PREDICATE'.
If both attributes are nil or equals the next strategy in `STRATEGIES' is used
to compare."
  (let ((arg1 (get-text-property 0 attribute entry1))
        (arg2 (get-text-property 0 attribute entry2)))
    (cond
     ((or (and (null arg1) (null arg2)) (equal arg1 arg2))
      (apply (dashboard-agenda--build-sort-function strategies) (list entry1 entry2)))
     ((null arg1) nil)
     ((null arg2) t)
     (t (apply predicate (list arg1 arg2))))))

(defun dashboard-agenda-insert (list-size)
  "Insert dashboard-agenda LIST-SIZE in dashboard."
  (dashboard-insert-section
   (if dashboard-week-agenda
       "Agenda for the coming week:"
     "Agenda for today:")
   (dashboard-agenda--sorted-agenda)
   list-size
   'agenda
   (dashboard-get-shortcut 'agenda)
   `(lambda (&rest _)
      (let ((buffer (find-file-other-window
                     (get-text-property 0 'dashboard-agenda-file ,el))))
        (with-current-buffer buffer
          (goto-char (get-text-property 0 'dashboard-agenda-loc ,el))
          (switch-to-buffer buffer))))
   (format "%s" el)))

(provide 'dashboard-agenda)
;;; dashboard-agenda.el ends here
