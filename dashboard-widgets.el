;;; dashboard-widgets.el --- A startup screen extracted from Spacemacs  -*- lexical-binding: t -*-

;; Copyright (c) 2016-2024 emacs-dashboard maintainers

;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:

;; An extensible Emacs dashboard, with sections for
;; bookmarks, projects (projectile or project.el), org-agenda and more.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'subr-x)

;;
;;; Externals

;; Compiler pacifier
(declare-function all-the-icons-icon-for-dir "ext:all-the-icons.el")
(declare-function all-the-icons-icon-for-file "ext:all-the-icons.el")
(declare-function all-the-icons-fileicon "ext:data-fileicons.el")
(declare-function all-the-icons-octicon "ext:data-octicons.el")
(declare-function nerd-icons-icon-for-dir "ext:nerd-icons.el")
(declare-function nerd-icons-icon-for-file "ext:nerd-icons.el")
(declare-function nerd-icons-sucicon "ext:nerd-icons.el")
(declare-function nerd-icons-octicon "ext:nerd-icons.el")
(declare-function nerd-icons-codicon "ext:nerd-icons.el")
(declare-function bookmark-get-filename "ext:bookmark.el")
(declare-function bookmark-all-names "ext:bookmark.el")
(declare-function calendar-date-compare "ext:calendar.el")
(declare-function projectile-cleanup-known-projects "ext:projectile.el")
(declare-function projectile-load-known-projects "ext:projectile.el")
(declare-function projectile-mode "ext:projectile.el")
;;; project.el in Emacs 26 does not contain this function
(declare-function project-known-project-roots "ext:project.el" nil t)
(declare-function project-forget-zombie-projects "ext:project.el" nil t)
(declare-function recentf-cleanup "ext:recentf.el")
(defvar all-the-icons-dir-icon-alist)
(defvar package-activated-list)
(defvar elpaca-after-init-time)
(declare-function string-pixel-width "subr-x.el")   ; TODO: remove this after 29.1
(declare-function shr-string-pixel-width "shr.el")  ; TODO: remove this after 29.1

(defvar recentf-list nil)

(defvar dashboard-buffer-name)

;;
;;; Customization

(defcustom dashboard-page-separator "\n\n"
  "Separator to use between the different pages."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-image-banner-max-height 0
  "Maximum height of banner image.

This setting applies only if Emacs supports image transforms or
compiled with Imagemagick support.  When value is non-zero the image
banner will be resized to the specified height in pixels, with aspect
ratio preserved."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-image-banner-max-width 0
  "Maximum width of banner image.

This setting applies if Emacs supports image transforms or compiled
with Imagemagick support.  When value is non-zero the image banner
will be resized to the specified width in pixels, with aspect ratio
preserved."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-image-extra-props nil
  "Additional image attributes to assign to the image.
This could be useful for displaying images with transparency,
for example, by setting the `:mask' property to `heuristic'.
See `create-image' and Info node `(elisp)Image Descriptors'."
  :type 'plist
  :group 'dashboard)

(defcustom dashboard-set-heading-icons nil
  "When non nil, heading sections will have icons."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-set-file-icons nil
  "When non nil, file lists will have icons."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-set-navigator nil
  "When non nil, a navigator will be displayed under the banner."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-set-init-info t
  "When non nil, init info will be displayed under the banner."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-set-footer t
  "When non nil, a footer will be displayed at the bottom."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-footer-messages
  '("The one true editor, Emacs!"
    "Who the hell uses VIM anyway? Go Evil!"
    "Free as free speech, free as free Beer"
    "Happy coding!"
    "Vi Vi Vi, the editor of the beast"
    "Welcome to the church of Emacs"
    "While any text editor can save your files, only Emacs can save your soul"
    "I showed you my source code, pls respond")
  "A list of messages, one of which dashboard chooses to display."
  :type 'list
  :group 'dashboard)

(defcustom dashboard-icon-type (and (or dashboard-set-heading-icons
                                        dashboard-set-file-icons)
                                    (or (require 'nerd-icons nil t)
                                        (require 'all-the-icons nil t)))
  "Icon type used for dashboard.
The value can be one of: `all-the-icons', `nerd-icons'."
  :type 'symbol
  :group 'dashboard
  :set
  (lambda (k v)
    (pcase v
      ('all-the-icons
       (unless (require 'all-the-icons nil t)
         (setq v nil)))
      ('nerd-icons
       (unless (require 'nerd-icons nil t)
         (setq v nil))))
    (set k v)))

(defcustom dashboard-heading-icons
  (pcase dashboard-icon-type
    ('all-the-icons '((recents   . "history")
                      (bookmarks . "bookmark")
                      (agenda    . "calendar")
                      (projects  . "rocket")
                      (registers . "database")))
    ('nerd-icons '((recents   . "nf-oct-history")
                   (bookmarks . "nf-oct-bookmark")
                   (agenda    . "nf-oct-calendar")
                   (projects  . "nf-oct-rocket")
                   (registers . "nf-oct-database"))))
  "Association list for the icons of the heading sections.
Will be of the form `(list-type . icon-name-string)`.
If nil it is disabled.  Possible values for list-type are:
`recents' `bookmarks' `projects' `agenda' `registers'"
  :type  '(repeat (alist :key-type symbol :value-type string))
  :group 'dashboard)

(defcustom dashboard-heading-icon-height 1.2
  "The height of the heading icon."
  :type 'float
  :group 'dashboard)

(defcustom dashboard-heading-icon-v-adjust 0.0
  "The v-adjust of the heading icon."
  :type 'float
  :group 'dashboard)

(defcustom dashboard-agenda-item-icon
  (pcase dashboard-icon-type
    ('all-the-icons (all-the-icons-octicon "primitive-dot" :height 1.0 :v-adjust 0.01))
    ('nerd-icons (nerd-icons-octicon "nf-oct-dot_fill" :height 1.0 :v-adjust 0.01)))
  "Agenda item icon."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-remote-path-icon
  (pcase dashboard-icon-type
    ('all-the-icons (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
    ('nerd-icons (nerd-icons-codicon "nf-cod-radio_tower" :height 1.0 :v-adjust 0.01)))
  "Remote path icon."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-show-shortcuts t
  "Whether to show shortcut keys for each section."
  :type 'boolean
  :group 'dashboard)

(defconst dashboard-banners-directory
  (concat (file-name-directory (locate-library "dashboard")) "banners/")
  "Default banner directory.")

(defconst dashboard-banner-official-png
  (concat dashboard-banners-directory "emacs.png")
  "Emacs banner image.")

(defconst dashboard-banner-logo-png
  (concat dashboard-banners-directory "logo.png")
  "Emacs banner image.")

(defcustom dashboard-banner-logo-title "Welcome to Emacs!"
  "Specify the startup banner."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-banner-ascii "EMACS"
  "String to be shown in place of the startup banner
if `dashboard-startup-banner' is set to `ascii'."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-navigator-buttons nil
  "Specify the navigator buttons.
The format is: `icon title help action face prefix suffix`.

Example:
`((\"☆\" \"Star\" \"Show stars\" (lambda (&rest _)
                                    (show-stars)) warning \"[\" \"]\"))"
  :type '(repeat (repeat (list string string string function symbol string string)))
  :group 'dashboard)

(defcustom dashboard-init-info
  (lambda ()
    (let ((package-count 0) (time (emacs-init-time)))
      (when (bound-and-true-p package-alist)
        (setq package-count (length package-activated-list)))
      (when (boundp 'straight--profile-cache)
        (setq package-count (+ (hash-table-count straight--profile-cache) package-count)))
      (when (fboundp 'elpaca--queued)
        (setq time (format "%f seconds" (float-time (time-subtract elpaca-after-init-time
                                                                   before-init-time))))
        (setq package-count (length (elpaca--queued))))
      (if (zerop package-count)
          (format "Emacs started in %s" time)
        (format "%d packages loaded in %s" package-count time))))
  "Init info with packages loaded and init time."
  :type '(function string)
  :group 'dashboard)

(defcustom dashboard-display-icons-p #'display-graphic-p
  "Predicate to determine whether dashboard should show icons.
Can be nil to not show icons and any truthy value to show them.  When set to a
function the result of the function will be interpreted as the predicate value."
  :type '(choice (function :tag "Predicate function")
                 (boolean :tag "Predicate value"))
  :group 'dashboard)

(defun dashboard-replace-displayable (str &optional rep)
  "Replace non-displayable character from STR.

Optional argument REP is the replacement string of non-displayable character."
  (if (stringp str)
      (let ((rep (or rep ""))
            (results (list)))
        (dolist (string (split-string str ""))
          (let* ((char (string-to-char string))
                 (string (if (char-displayable-p char)
                             string
                           rep)))
            (push string results)))
        (string-join (reverse results)))
    ""))

(defun dashboard-display-icons-p ()
  "Assert whether to show icons based on the `dashboard-display-icons-p' variable."
  (if (functionp dashboard-display-icons-p)
      (funcall dashboard-display-icons-p)
    dashboard-display-icons-p))

(defun dashboard-icon-for-dir (dir &rest args)
  "Get the formatted icon for DIR.
ARGS should be a plist containing `:height', `:v-adjust',
or `:face' properties."
  (dashboard-replace-displayable
   (pcase dashboard-icon-type
     ('all-the-icons (apply #'all-the-icons-icon-for-dir dir args))
     ('nerd-icons (apply #'nerd-icons-icon-for-dir dir args)))))

(defun dashboard-icon-for-file (file &rest args)
  "Get the formatted icon for FILE.
ARGS should be a plist containing `:height', `:v-adjust', or `:face' properties."
  (dashboard-replace-displayable
   (pcase dashboard-icon-type
     ('all-the-icons (apply #'all-the-icons-icon-for-file file args))
     ('nerd-icons (apply #'nerd-icons-icon-for-file file args)))))

(defun dashboard-octicon (name &rest args)
  "Get the formatted octicon by NAME.
ARGS should be a plist containing `:height', `:v-adjust', or `:face' properties."
  (dashboard-replace-displayable
   (pcase dashboard-icon-type
     ('all-the-icons (apply #'all-the-icons-octicon name args))
     ('nerd-icons (apply #'nerd-icons-octicon name args)))))

(defcustom dashboard-footer-icon
  (if (dashboard-display-icons-p)
      (pcase dashboard-icon-type
        ('all-the-icons
         (all-the-icons-fileicon "emacs"
                                 :height 1.1
                                 :v-adjust -0.05
                                 :face 'dashboard-footer-icon-face))
        ('nerd-icons
         (nerd-icons-sucicon "nf-custom-emacs"
                             :height 1.1
                             :v-adjust -0.05
                             :face 'dashboard-footer-icon-face)))
    (propertize ">" 'face 'dashboard-footer-icon-face))
  "Footer's icon."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-startup-banner 'official
  "Specify the startup banner.
Default value is `official', it displays the Emacs logo.  `logo' displays Emacs
alternative logo.  If set to `ascii', the value of `dashboard-banner-ascii'
will be used as the banner.  An integer value is the index of text banner.
A string value must be a path to a .PNG or .TXT file.  If the value is
nil then no banner is displayed."
  :type '(choice (const   :tag "no banner" nil)
                 (const   :tag "offical"   official)
                 (const   :tag "logo"      logo)
                 (const   :tag "ascii"     ascii)
                 (integer :tag "index of a text banner")
                 (string  :tag "a path to an image or text banner")
                 (cons    :tag "an image and text banner"
                          (string :tag "image banner path")
                          (string :tag "text banner path")))
  :group 'dashboard)

(defcustom dashboard-item-generators
  '((recents   . dashboard-insert-recents)
    (bookmarks . dashboard-insert-bookmarks)
    (projects  . dashboard-insert-projects)
    (agenda    . dashboard-insert-agenda)
    (registers . dashboard-insert-registers))
  "Association list of items to how to generate in the startup buffer.
Will be of the form `(list-type . list-function)'.
Possible values for list-type are: `recents', `bookmarks', `projects',
`agenda' ,`registers'."
  :type  '(repeat (alist :key-type symbol :value-type function))
  :group 'dashboard)

(defcustom dashboard-projects-backend 'projectile
  "The package that supplies the list of recent projects.
With the value `projectile', the projects widget uses the package
projectile (available in MELPA).  With the value `project-el',
the widget uses the package project (available in GNU ELPA).

To activate the projects widget, add e.g. `(projects . 10)' to
`dashboard-items' after making sure the necessary package is
installed."
  :type '(choice (const :tag "Use projectile" projectile)
                 (const :tag "Use project.el" project-el))
  :group 'dashboard)

(defcustom dashboard-remove-missing-entry nil
  "If non-nil, try to remove missing entries."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-items
  '((recents   . 5)
    (bookmarks . 5)
    (agenda    . 5))
  "Association list of items to show in the startup buffer.
Will be of the form `(list-type . list-size)'.
If nil it is disabled.  Possible values for list-type are:
`recents' `bookmarks' `projects' `agenda' `registers'."
  :type  '(repeat (alist :key-type symbol :value-type integer))
  :group 'dashboard)

(defcustom dashboard-item-shortcuts
  '((recents   . "r")
    (bookmarks . "m")
    (projects  . "p")
    (agenda    . "a")
    (registers . "e"))
  "Association list of items and their corresponding shortcuts.
Will be of the form `(list-type . keys)' as understood by `(kbd keys)'.
If nil, shortcuts are disabled.  If an entry's value is nil, that item's
shortcut is disbaled.  See `dashboard-items' for possible values of list-type.'"
  :type '(repeat (alist :key-type symbol :value-type string))
  :group 'dashboard)

(defcustom dashboard-item-names nil
  "Association list of item heading names.
When an item is nil or not present, the default name is used.
Will be of the form `(default-name . new-name)'."
  :type '(alist :key-type string :value-type string)
  :options '("Recent Files:" "Bookmarks:" "Agenda for today:"
             "Agenda for the coming week:" "Registers:" "Projects:")
  :group 'dashboard)

(defcustom dashboard-items-default-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded."
  :type  'integer
  :group 'dashboard)

(defcustom dashboard-path-style nil
  "Style to display path."
  :type '(choice
          (const :tag "No specify" nil)
          (const :tag "Truncate the beginning part of the path" truncate-beginning)
          (const :tag "Truncate the middle part of the path" truncate-middle)
          (const :tag "Truncate the end part of the path" truncate-end))
  :group 'dashboard)

(defcustom dashboard-path-max-length 70
  "Maximum length for path to display."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-path-shorten-string "..."
  "String the that displays in the center of the path."
  :type 'string
  :group 'dashboard)

;;
;;; Faces

(defface dashboard-text-banner
  '((t (:inherit font-lock-keyword-face)))
  "Face used for text banners."
  :group 'dashboard)

(defface dashboard-banner-logo-title
  '((t :inherit default))
  "Face used for the banner title."
  :group 'dashboard)

(defface dashboard-navigator
  '((t (:inherit font-lock-keyword-face)))
  "Face used for the navigator."
  :group 'dashboard)

(defface dashboard-heading
  '((t (:inherit font-lock-keyword-face)))
  "Face used for widget headings."
  :group 'dashboard)

(defface dashboard-items-face
  '((t (:inherit widget-button)))
  "Face used for items."
  :group 'dashboard)

(defface dashboard-no-items-face
  '((t (:inherit widget-button)))
  "Face used for no items."
  :group 'dashboard)

(defface dashboard-footer-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for footer text."
  :group 'dashboard)

(defface dashboard-footer-icon-face
  '((t (:inherit dashboard-footer-face)))
  "Face used for icon in footer."
  :group 'dashboard)

(define-obsolete-face-alias
 'dashboard-text-banner-face 'dashboard-text-banner "1.2.6")
(define-obsolete-face-alias
 'dashboard-banner-logo-title-face 'dashboard-banner-logo-title "1.2.6")
(define-obsolete-face-alias
 'dashboard-heading-face 'dashboard-heading "1.2.6")

;;
;;; Util

(defmacro dashboard-mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defun dashboard-funcall-fboundp (fnc &rest args)
  "Call FNC with ARGS if exists."
  (when (fboundp fnc) (if args (funcall fnc args) (funcall fnc))))

;; TODO: Use function `string-pixel-width' after 29.1
(defun dashboard-string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp #'string-pixel-width)
      (string-pixel-width str)
    (require 'shr)
    (shr-string-pixel-width str)))

(defun dashboard-str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (dashboard-string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

;;
;;; Widget helpers

(defun dashboard-subseq (seq end)
  "Return the subsequence of SEQ from 0 to END."
  (let ((len (length seq))) (butlast seq (- len (min len end)))))

(defun dashboard-get-shortcut-name (item)
  "Get the shortcut name to be used for ITEM."
  (let ((elem (rassoc item dashboard-item-shortcuts)))
    (and elem (car elem))))

(defun dashboard-get-shortcut (item)
  "Get the shortcut to be used for ITEM."
  (let ((elem (assq item dashboard-item-shortcuts)))
    (and elem (cdr elem))))

(defmacro dashboard-insert-shortcut (shortcut-id
                                     shortcut-char
                                     search-label
                                     &optional no-next-line)
  "Insert a shortcut SHORTCUT-CHAR for a given SEARCH-LABEL.

SHORTCUT-ID is the section identifier.

Optionally, provide NO-NEXT-LINE to move the cursor forward a line."
  (let* (;; Ensure punctuation and upper case in search string is not
         ;; used to construct the `defun'
         (name (downcase (replace-regexp-in-string "[[:punct:]]+" "" (format "%s" search-label))))
         ;; remove symbol quote
         (sym (intern (replace-regexp-in-string "'" "" (format "dashboard-jump-to-%s" shortcut-id)))))
    `(progn
       (eval-when-compile (defvar dashboard-mode-map))
       (defun ,sym nil
         ,(concat "Jump to " name ".  This code is dynamically generated in `dashboard-insert-shortcut'.")
         (interactive)
         (unless (search-forward ,search-label (point-max) t)
           (search-backward ,search-label (point-min) t))
         ,@(unless no-next-line '((forward-line 1)))
         (back-to-indentation))
       (eval-after-load 'dashboard
         (define-key dashboard-mode-map ,shortcut-char ',sym)))))

(defun dashboard-append (msg &optional _messagebuf)
  "Append MSG to dashboard buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create dashboard-buffer-name)
    (goto-char (point-max))
    (let ((inhibit-read-only t)) (insert msg))))

(defun dashboard-modify-heading-icons (alist)
  "Append ALIST items to `dashboard-heading-icons' to modify icons."
  (dolist (icon alist)
    (add-to-list 'dashboard-heading-icons icon)))

(defun dashboard-insert-page-break ()
  "Insert a page break line in dashboard buffer."
  (dashboard-append dashboard-page-separator))

(defun dashboard-insert-heading (heading &optional shortcut icon)
  "Insert a widget HEADING in dashboard buffer, adding SHORTCUT, ICON if provided."
  (when (and (dashboard-display-icons-p) dashboard-set-heading-icons)
    (let ((args `( :height   ,dashboard-heading-icon-height
                   :v-adjust ,dashboard-heading-icon-v-adjust
                   :face     dashboard-heading)))
      (insert
       (pcase heading
         ("Recent Files:"
          (apply #'dashboard-octicon (cdr (assoc 'recents dashboard-heading-icons)) args))
         ("Bookmarks:"
          (apply #'dashboard-octicon (cdr (assoc 'bookmarks dashboard-heading-icons)) args))
         ((or "Agenda for today:"
              "Agenda for the coming week:")
          (apply #'dashboard-octicon (cdr (assoc 'agenda dashboard-heading-icons)) args))
         ("Registers:"
          (apply #'dashboard-octicon (cdr (assoc 'registers dashboard-heading-icons)) args))
         ("Projects:"
          (apply #'dashboard-octicon (cdr (assoc 'projects dashboard-heading-icons)) args))
         ("List Directories:"
          (apply #'dashboard-octicon (cdr (assoc 'ls-directories dashboard-heading-icons)) args))
         ("List Files:"
          (apply #'dashboard-octicon (cdr (assoc 'ls-files dashboard-heading-icons)) args))
         (_
          (if (null icon) " " icon))))
      (insert " ")))

  (insert (propertize heading 'face 'dashboard-heading))

  ;; Turn the inserted heading into an overlay, so that we may freely change
  ;; its name without breaking any of the functions that expect the default name.
  ;; If there isn't a suitable entry in `dashboard-item-names',
  ;; we fallback to using HEADING.  In that case we still want it to be an
  ;; overlay to maintain consistent behavior (such as the point movement)
  ;; between modified and default headings.
  (let ((ov (make-overlay (- (point) (length heading)) (point) nil t)))
    (overlay-put ov 'display (or (cdr (assoc heading dashboard-item-names)) heading))
    (overlay-put ov 'face 'dashboard-heading))
  (when shortcut (insert (format " (%s)" shortcut))))

(defun dashboard-center-text (start end)
  "Center the text between START and END."
  (save-excursion
    (goto-char start)
    (let ((width 0))
      (while (< (point) end)
        (let* ((line-str (buffer-substring (line-beginning-position) (line-end-position)))
               (line-length (dashboard-str-len line-str)))
          (setq width (max width line-length)))
        (forward-line 1))
      (let ((prefix (propertize " " 'display `(space . (:align-to (- center ,(/ width 2)))))))
        (add-text-properties start end `(line-prefix ,prefix indent-prefix ,prefix))))))

(defun dashboard-insert-center (&rest strings)
  "Insert STRINGS in the center of the buffer."
  (let ((start (point)))
    (apply #'insert strings)
    (dashboard-center-text start (point))))

;;
;;; Banner

(defun dashboard-get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat dashboard-banners-directory (format "%d.txt" index)))

(defun dashboard--image-supported-p (img)
  "Return non-nil if IMG exists and is a supported image type."
  ;; In Emacs 29.1 we could use `image-supported-file-p'. However:
  ;; - We need to support Emacs 26.
  ;; - That function will only look at filenames, this one will inspect the file data itself.
  (and (file-exists-p img) (ignore-errors (image-type-available-p (image-type img)))))

(defun dashboard-choose-banner ()
  "Return a plist specifying the chosen banner based on `dashboard-startup-banner'."
  (pcase dashboard-startup-banner
    ('nil nil)
    ('official
     (append (when (image-type-available-p 'png)
               (list :image dashboard-banner-official-png))
             (list :text (dashboard-get-banner-path 1))))
    ('logo
     (append (when (image-type-available-p 'png)
               (list :image dashboard-banner-logo-png))
             (list :text (dashboard-get-banner-path 1))))
    ('ascii
     (append (list :text dashboard-banner-ascii)))
    ((pred integerp)
     (list :text (dashboard-get-banner-path dashboard-startup-banner)))
    ((pred stringp)
     (pcase dashboard-startup-banner
       ((pred (lambda (f) (not (file-exists-p f))))
        (message "could not find banner %s, use default instead" dashboard-startup-banner)
        (list :text (dashboard-get-banner-path 1)))
       ((pred (string-suffix-p ".txt"))
        (list :text (if (file-exists-p dashboard-startup-banner)
                        dashboard-startup-banner
                      (message "could not find banner %s, use default instead" dashboard-startup-banner)
                      (dashboard-get-banner-path 1))))
       ((pred dashboard--image-supported-p)
        (list :image dashboard-startup-banner
              :text (dashboard-get-banner-path 1)))
       (_
        (message "unsupported file type %s" (file-name-nondirectory dashboard-startup-banner))
        (list :text (dashboard-get-banner-path 1)))))
    (`(,img . ,txt)
     (list :image (if (dashboard--image-supported-p img)
                      img
                    (message "could not find banner %s, use default instead" img)
                    dashboard-banner-official-png)
           :text (if (and (file-exists-p txt) (string-suffix-p ".txt" txt))
                     txt
                   (message "could not find banner %s, use default instead" txt)
                   (dashboard-get-banner-path 1))))
    (_
     (message "unsupported banner config %s" dashboard-startup-banner))))

(defun dashboard--type-is-gif-p (image-path)
  "Return if image is a gif.
String -> bool.
Argument IMAGE-PATH path to the image."
  (eq 'gif (image-type image-path)))

(defun dashboard--type-is-xbm-p (image-path)
  "Return if image is a xbm.
String -> bool.
Argument IMAGE-PATH path to the image."
  (eq 'xbm (image-type image-path)))

(defun dashboard-insert-banner ()
  "Insert the banner at the top of the dashboard."
  (goto-char (point-max))
  (when-let (banner (dashboard-choose-banner))
    (insert "\n")
    (let ((start (point))
          buffer-read-only
          text-width
          image-spec)
      (when (display-graphic-p) (insert "\n"))
      ;; If specified, insert a text banner.
      (when-let (txt (plist-get banner :text))
        (if (eq dashboard-startup-banner 'ascii)
            (save-excursion (insert txt))
          (insert-file-contents txt))
        (put-text-property (point) (point-max) 'face 'dashboard-text-banner)
        (setq text-width 0)
        (while (not (eobp))
          (let ((line-length (- (line-end-position) (line-beginning-position))))
            (if (< text-width line-length)
                (setq text-width line-length)))
          (forward-line 1)))
      ;; If specified, insert an image banner. When displayed in a graphical frame, this will
      ;; replace the text banner.
      (when-let (img (plist-get banner :image))
        (let ((img-props
               (append (when (> dashboard-image-banner-max-width 0)
                         (list :max-width dashboard-image-banner-max-width))
                       (when (> dashboard-image-banner-max-height 0)
                         (list :max-height dashboard-image-banner-max-height))
                       dashboard-image-extra-props)))
          (setq image-spec
                (cond ((dashboard--type-is-gif-p img)
                       (create-image img))
                      ((dashboard--type-is-xbm-p img)
                       (create-image img))
                      ((image-type-available-p 'imagemagick)
                       (apply 'create-image img 'imagemagick nil img-props))
                      (t
                       (apply 'create-image img nil nil
                              (when (and (fboundp 'image-transforms-p)
                                         (memq 'scale (funcall 'image-transforms-p)))
                                img-props))))))
        (add-text-properties start (point) `(display ,image-spec))
        (when (dashboard--type-is-gif-p img) (image-animate image-spec 0 t)))
      ;; Finally, center the banner (if any).
      (when-let* ((text-align-spec `(space . (:align-to (- center ,(/ text-width 2)))))
                  (image-align-spec `(space . (:align-to (- center (0.5 . ,image-spec)))))
                  (prop
                   (cond
                    ;; Both an image & text banner.
                    ((and image-spec text-width)
                     ;; The quoting is intentional. This is a conditional display spec that will
                     ;; align the banner at redisplay time.
                     `((when (display-graphic-p) . ,image-align-spec)
                       (when (not (display-graphic-p)) . ,text-align-spec)))
                    ;; One or the other.
                    (text-width text-align-spec)
                    (image-spec image-align-spec)
                    ;; No banner.
                    (t nil)))
                  (prefix (propertize " " 'display prop)))
        (add-text-properties start (point) `(line-prefix ,prefix wrap-prefix ,prefix)))
      (insert "\n\n")
      (add-text-properties start (point) '(cursor-intangible t inhibit-isearch t))))
  (when dashboard-banner-logo-title
    (dashboard-insert-center (propertize dashboard-banner-logo-title 'face 'dashboard-banner-logo-title))
    (insert "\n\n"))
  (dashboard-insert-navigator)
  (dashboard-insert-init-info))

;;
;;; Initialize info

(defun dashboard-insert-init-info ()
  "Insert init info when `dashboard-set-init-info' is t."
  (when dashboard-set-init-info
    (let ((init-info (if (functionp dashboard-init-info)
                         (funcall dashboard-init-info)
                       dashboard-init-info)))
      (dashboard-insert-center (propertize init-info 'face 'font-lock-comment-face)))))

(defun dashboard-insert-navigator ()
  "Insert Navigator of the dashboard."
  (when (and dashboard-set-navigator dashboard-navigator-buttons)
    (dolist (line dashboard-navigator-buttons)
      (dolist (btn line)
        (let* ((icon (car btn))
               (title (cadr btn))
               (help (or (cadr (cdr btn)) ""))
               (action (or (cadr (cddr btn)) #'ignore))
               (face (or (cadr (cddr (cdr btn))) 'dashboard-navigator))
               (prefix (or (cadr (cddr (cddr btn))) (propertize "[" 'face face)))
               (suffix (or (cadr (cddr (cddr (cdr btn)))) (propertize "]" 'face face))))
          (widget-create 'item
                         :tag (concat
                               (when icon
                                 (propertize icon 'face
                                             (let ((prop-face (get-text-property 0 'face icon)))
                                               (if prop-face
                                                   `(:inherit ,prop-face :inherit ,face)
                                                 `(:inherit ,face)))))
                               (when (and icon title
                                          (not (string-equal icon ""))
                                          (not (string-equal title "")))
                                 (propertize " " 'face 'variable-pitch))
                               (when title (propertize title 'face face)))
                         :help-echo help
                         :action action
                         :button-face 'dashboard-items-face
                         :mouse-face 'highlight
                         :button-prefix prefix
                         :button-suffix suffix
                         :format "%[%t%]")
          (insert " ")))
      (dashboard-center-text (line-beginning-position) (line-end-position))
      (insert "\n"))
    (insert "\n")))

(defmacro dashboard-insert-section (section-name list list-size shortcut-id shortcut-char action &rest widget-params)
  "Add a section with SECTION-NAME and LIST of LIST-SIZE items to the dashboard.

SHORTCUT-ID is the section identifier.
SHORTCUT-CHAR is the keyboard shortcut used to access the section.
ACTION is theaction taken when the user activates the widget button.
WIDGET-PARAMS are passed to the \"widget-create\" function."
  `(progn
     (dashboard-insert-heading ,section-name
                               (if (and ,list ,shortcut-char dashboard-show-shortcuts) ,shortcut-char))
     (if ,list
         (when (and (dashboard-insert-section-list
                     ,section-name
                     (dashboard-subseq ,list ,list-size)
                     ,action
                     ,@widget-params)
                    ,shortcut-id ,shortcut-char)
           (dashboard-insert-shortcut ,shortcut-id ,shortcut-char ,section-name))
       (insert (propertize "\n    --- No items ---" 'face 'dashboard-no-items-face)))))

;;
;;; Section list

(defmacro dashboard-insert-section-list (section-name list action &rest rest)
  "Insert into SECTION-NAME a LIST of items, expanding ACTION and passing REST
to widget creation."
  `(when (car ,list)
     (mapc
      (lambda (el)
        (let ((tag ,@rest))
          (insert "\n    ")

          (when (and (dashboard-display-icons-p)
                     dashboard-set-file-icons)
            (let* ((path (car (last (split-string ,@rest " - "))))
                   (icon (if (and (not (file-remote-p path))
                                  (file-directory-p path))
                             (dashboard-icon-for-dir path nil "")
                           (cond
                            ((or (string-equal ,section-name "Agenda for today:")
                                 (string-equal ,section-name "Agenda for the coming week:"))
                             dashboard-agenda-item-icon)
                            ((file-remote-p path)
                             dashboard-remote-path-icon)
                            (t (dashboard-icon-for-file (file-name-nondirectory path)
                                                        :v-adjust -0.05))))))
              (setq tag (concat icon " " ,@rest))))

          (widget-create 'item
                         :tag tag
                         :action ,action
                         :button-face 'dashboard-items-face
                         :mouse-face 'highlight
                         :button-prefix ""
                         :button-suffix ""
                         :format "%[%t%]")))
      ,list)))

;;
;;; Footer

(defun dashboard-random-footer ()
  "Return a random footer from `dashboard-footer-messages'."
  (nth (random (length dashboard-footer-messages)) dashboard-footer-messages))

(defun dashboard-insert-footer ()
  "Insert footer of dashboard."
  (when-let ((footer (and dashboard-set-footer (dashboard-random-footer)))
             (footer-icon (dashboard-replace-displayable dashboard-footer-icon)))
    (insert "\n")
    (dashboard-insert-center
     (if (string-empty-p footer-icon) footer-icon
       (concat footer-icon " "))
     (propertize footer 'face 'dashboard-footer-face)
     "\n")))

;;
;;; Truncate

(defcustom dashboard-shorten-by-window-width nil
  "Shorten path by window edges."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-shorten-path-offset 0
  "Shorten path offset on the edges."
  :type 'integer
  :group 'dashboard)

(defun dashboard-f-filename (path)
  "Return file name from PATH."
  (file-name-nondirectory path))

(defun dashboard-f-base (path)
  "Return directory name from PATH."
  (file-name-nondirectory (directory-file-name (file-name-directory path))))

(defun dashboard-shorten-path-beginning (path)
  "Shorten PATH from beginning if exceeding maximum length."
  (let* ((len-path (length path))
         (slen-path (dashboard-str-len path))
         (len-rep (dashboard-str-len dashboard-path-shorten-string))
         (len-total (- dashboard-path-max-length len-rep))
         front)
    (if (<= slen-path dashboard-path-max-length) path
      (setq front (ignore-errors (substring path (- slen-path len-total) len-path)))
      (if front (concat dashboard-path-shorten-string front) ""))))

(defun dashboard-shorten-path-middle (path)
  "Shorten PATH from middle if exceeding maximum length."
  (let* ((len-path (length path))
         (slen-path (dashboard-str-len path))
         (len-rep (dashboard-str-len dashboard-path-shorten-string))
         (len-total (- dashboard-path-max-length len-rep))
         (center (/ len-total 2))
         (end-back center)
         (start-front (- slen-path center))
         back front)
    (if (<= slen-path dashboard-path-max-length) path
      (setq back (substring path 0 end-back)
            front (ignore-errors (substring path start-front len-path)))
      (if front (concat back dashboard-path-shorten-string front) ""))))

(defun dashboard-shorten-path-end (path)
  "Shorten PATH from end if exceeding maximum length."
  (let* ((len-path (length path))
         (slen-path (dashboard-str-len path))
         (len-rep (dashboard-str-len dashboard-path-shorten-string))
         (diff (- slen-path len-path))
         (len-total (- dashboard-path-max-length len-rep diff))
         back)
    (if (<= slen-path dashboard-path-max-length) path
      (setq back (ignore-errors (substring path 0 len-total)))
      (if (and back (< 0 dashboard-path-max-length))
          (concat back dashboard-path-shorten-string) ""))))

(defun dashboard--get-base-length (path type)
  "Return the length of the base from the PATH by TYPE."
  (let* ((is-dir (file-directory-p path))
         (base (if is-dir (dashboard-f-base path) (dashboard-f-filename path)))
         (option (cl-case type
                   (recents 'dashboard-recentf-show-base)
                   (bookmarks 'dashboard-bookmarks-show-base)
                   (projects 'dashboard-projects-show-base)))
         (option-val (symbol-value option))
         base-len)
    (cl-case option-val
      (`align (setq base-len (dashboard--align-length-by-type type)))
      (`nil (setq base-len 0))
      (t (setq base-len (length base))))
    base-len))

(defun dashboard-shorten-path (path type)
  "Shorten the PATH by TYPE."
  (setq path (abbreviate-file-name path))
  (let ((dashboard-path-max-length
         (if (and dashboard-path-style dashboard-shorten-by-window-width)
             (- (window-width) (dashboard--get-base-length path type)
                dashboard-shorten-path-offset)
           dashboard-path-max-length)))
    (cl-case dashboard-path-style
      (truncate-beginning (dashboard-shorten-path-beginning path))
      (truncate-middle (dashboard-shorten-path-middle path))
      (truncate-end (dashboard-shorten-path-end path))
      (t path))))

(defun dashboard-shorten-paths (paths alist type)
  "Shorten all path from PATHS by TYPE and store it to ALIST."
  (let (lst-display abbrev (index 0))
    (setf (symbol-value alist) nil)  ; reset
    (dolist (item paths)
      (setq abbrev (dashboard-shorten-path item type)
            ;; Add salt here, and use for extraction.
            ;; See function `dashboard-extract-key-path-alist'.
            abbrev (format "%s|%s" index abbrev))
      ;; store `abbrev' as id; and `item' with value
      (push (cons abbrev item) (symbol-value alist))
      (push abbrev lst-display)
      (cl-incf index))
    (reverse lst-display)))

(defun dashboard-extract-key-path-alist (key alist)
  "Remove salt from KEY, and return true shorten path from ALIST."
  (let* ((key (car (assoc key alist))) (split (split-string key "|")))
    (nth 1 split)))

(defun dashboard-expand-path-alist (key alist)
  "Get the full path (un-shorten) using KEY from ALIST."
  (cdr (assoc key alist)))

(defun dashboard--generate-align-format (fmt len)
  "Return FMT after inserting align LEN."
  (let ((pos (1+ (string-match-p "%s" fmt))))
    (concat (substring fmt 0 pos)
            (concat "-" (number-to-string len))
            (substring fmt pos (length fmt)))))

(defun dashboard--align-length-by-type (type)
  "Return the align length by TYPE of the section."
  (let ((len-item (cdr (assoc type dashboard-items))) (count 0) (align-length -1)
        len-list base)
    (cl-case type
      (`recents
       (require 'recentf)
       (setq len-list (length recentf-list))
       (while (and (< count len-item) (< count len-list))
         (setq base (nth count recentf-list)
               align-length (max align-length (dashboard-str-len (dashboard-f-filename base))))
         (cl-incf count)))
      (`bookmarks
       (let ((bookmarks-lst (bookmark-all-names)))
         (setq len-list (length bookmarks-lst))
         (while (and (< count len-item) (< count len-list))
           (setq base (nth count bookmarks-lst)
                 align-length (max align-length (dashboard-str-len base)))
           (cl-incf count))))
      (`projects
       (let ((projects-lst (dashboard-projects-backend-load-projects)))
         (setq len-list (length projects-lst))
         (while (and (< count len-item) (< count len-list))
           (setq base (nth count projects-lst)
                 align-length (max align-length (dashboard-str-len (dashboard-f-base base))))
           (cl-incf count))))
      (t (error "Unknown type for align length: %s" type)))
    align-length))

;;
;;; Recentf

(defcustom dashboard-recentf-show-base nil
  "Show the base file name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom dashboard-recentf-item-format "%s  %s"
  "Format to use when showing the base of the file name."
  :type 'string
  :group 'dashboard)

(defvar dashboard-recentf-alist nil
  "Alist records shorten's recent files and it's full paths.")

(defvar dashboard--recentf-cache-item-format nil
  "Cache to record the new generated align format.")

(defun dashboard-insert-recents (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (setq dashboard--recentf-cache-item-format nil)
  (dashboard-mute-apply
    (recentf-mode 1)
    (when dashboard-remove-missing-entry
      (ignore-errors (recentf-cleanup))))
  (dashboard-insert-section
   "Recent Files:"
   (dashboard-shorten-paths recentf-list 'dashboard-recentf-alist 'recents)
   list-size
   'recents
   (dashboard-get-shortcut 'recents)
   `(lambda (&rest _)
      (find-file-existing (dashboard-expand-path-alist ,el dashboard-recentf-alist)))
   (let* ((file (dashboard-expand-path-alist el dashboard-recentf-alist))
          (filename (dashboard-f-filename file))
          (path (dashboard-extract-key-path-alist el dashboard-recentf-alist)))
     (cl-case dashboard-recentf-show-base
       (`align
        (unless dashboard--recentf-cache-item-format
          (let* ((len-align (dashboard--align-length-by-type 'recents))
                 (new-fmt (dashboard--generate-align-format
                           dashboard-recentf-item-format len-align)))
            (setq dashboard--recentf-cache-item-format new-fmt)))
        (format dashboard--recentf-cache-item-format filename path))
       (`nil path)
       (t (format dashboard-recentf-item-format filename path))))))

;;
;;; Bookmarks

(defcustom dashboard-bookmarks-show-base t
  "Show the base file name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom dashboard-bookmarks-item-format "%s - %s"
  "Format to use when showing the base of the file name."
  :type 'string
  :group 'dashboard)

(defvar dashboard--bookmarks-cache-item-format nil
  "Cache to record the new generated align format.")

(defun dashboard-insert-bookmarks (list-size)
  "Add the list of LIST-SIZE items of bookmarks."
  (require 'bookmark)
  (dashboard-insert-section
   "Bookmarks:"
   (dashboard-subseq (bookmark-all-names) list-size)
   list-size
   'bookmarks
   (dashboard-get-shortcut 'bookmarks)
   `(lambda (&rest _) (bookmark-jump ,el))
   (if-let* ((filename el)
             (path (bookmark-get-filename el))
             (path-shorten (dashboard-shorten-path path 'bookmarks)))
       (cl-case dashboard-bookmarks-show-base
         (`align
          (unless dashboard--bookmarks-cache-item-format
            (let* ((len-align (dashboard--align-length-by-type 'bookmarks))
                   (new-fmt (dashboard--generate-align-format
                             dashboard-bookmarks-item-format len-align)))
              (setq dashboard--bookmarks-cache-item-format new-fmt)))
          (format dashboard--bookmarks-cache-item-format filename path-shorten))
         (`nil path-shorten)
         (t (format dashboard-bookmarks-item-format filename path-shorten)))
     el)))

;;
;;; Projects

(defcustom dashboard-projects-switch-function
  nil
  "Custom function to switch to projects from dashboard.
If non-NIL, should be bound to a function with one argument.  The function will
be called with the root directory of the project to switch to."
  :type '(choice (const :tag "Default" nil) function)
  :group 'dashboard)

(defcustom dashboard-projects-show-base nil
  "Show the project name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom dashboard-projects-item-format "%s  %s"
  "Format to use when showing the base of the project name."
  :type 'string
  :group 'dashboard)

(defvar dashboard-projects-alist nil
  "Alist records the shorten's project paths and it's full paths.")

(defvar dashboard--projects-cache-item-format nil
  "Cache to record the new generated align format.")

(defun dashboard-insert-projects (list-size)
  "Add the list of LIST-SIZE items of projects."
  (setq dashboard--projects-cache-item-format nil)
  (dashboard-insert-section
   "Projects:"
   (dashboard-shorten-paths
    (dashboard-subseq (dashboard-projects-backend-load-projects) list-size)
    'dashboard-projects-alist 'projects)
   list-size
   'projects
   (dashboard-get-shortcut 'projects)
   `(lambda (&rest _)
      (funcall (dashboard-projects-backend-switch-function)
               (dashboard-expand-path-alist ,el dashboard-projects-alist)))
   (let* ((file (dashboard-expand-path-alist el dashboard-projects-alist))
          (filename (dashboard-f-base file))
          (path (dashboard-extract-key-path-alist el dashboard-projects-alist)))
     (cl-case dashboard-projects-show-base
       (`align
        (unless dashboard--projects-cache-item-format
          (let* ((len-align (dashboard--align-length-by-type 'projects))
                 (new-fmt (dashboard--generate-align-format
                           dashboard-projects-item-format len-align)))
            (setq dashboard--projects-cache-item-format new-fmt)))
        (format dashboard--projects-cache-item-format filename path))
       (`nil path)
       (t (format dashboard-projects-item-format filename path))))))

(defun dashboard-projects-backend-load-projects ()
  "Depending on `dashboard-projects-backend' load corresponding backend.
Return function that returns a list of projects."
  (cl-case dashboard-projects-backend
    (`projectile
     (require 'projectile)
     (when dashboard-remove-missing-entry
       (dashboard-mute-apply
         (ignore-errors (projectile-cleanup-known-projects))))
     (projectile-load-known-projects))
    (`project-el
     (require 'project)
     (when dashboard-remove-missing-entry
       (dashboard-mute-apply
         (ignore-errors
           (dashboard-funcall-fboundp #'project-forget-zombie-projects))))
     (project-known-project-roots))
    (t
     (display-warning '(dashboard)
                      "Invalid value for `dashboard-projects-backend'"
                      :error))))

(defun dashboard-projects-backend-switch-function ()
  "Return the function to switch to a project.
Custom variable `dashboard-projects-switch-function' variable takes preference
over custom backends."
  (or dashboard-projects-switch-function
      (cl-case dashboard-projects-backend
        (`projectile 'projectile-switch-project-by-name)
        (`project-el
         (lambda (project)
           "This function is used to switch to `PROJECT'."
           (let ((default-directory project))
             (project-find-file))))
        (t
         (display-warning '(dashboard)
                          "Invalid value for `dashboard-projects-backend'"
                          :error)))))

;;
;;; Org Agenda

(require 'dashboard-agenda)

(defun dashboard-insert-agenda (list-size)
  "Add the list of LIST-SIZE items of agenda."
  (require 'org-mode)
  (require 'org-agenda)
  (warn "Emacs-dashboard: Agenda support will be moved to a different package.")

  (dashboard-agenda-insert list-size))

;;
;;; Registers

(defun dashboard-insert-registers (list-size)
  "Add the list of LIST-SIZE items of registers."
  (require 'register)
  (dashboard-insert-section
   "Registers:"
   register-alist
   list-size
   'registers
   (dashboard-get-shortcut 'registers)
   (lambda (&rest _) (jump-to-register (car el)))
   (format "%c - %s" (car el) (register-describe-oneline (car el)))))

(provide 'dashboard-widgets)
;;; dashboard-widgets.el ends here
