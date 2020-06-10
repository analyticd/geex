;;; geex-fontify.el --- Fontification for Freex
;;
;; Copyright (C) 2007 Per B. Sederberg, Greg Detre
;;
;; Author: Per B. Sederberg, Greg Detre
;; Keywords: hypermedia
;; Date:
;;
;; This file is part of Emacs Freex.  It is not part of GNU
;; Emacs.
;;
;; Emacs Freex is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any
;; later version.
;;
;; Emacs Freex is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with Emacs Freex; see the file COPYING.  If
;; not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;;
;;; Contributors:

;;; Code:

;; set the required files
(require 'geex)
(require 'geex-xml)

(defcustom geex-fontify-elements
  '(("olink" t t nil geex-fontify-element-olink)
    ("ulink" t t nil geex-fontify-element-ulink)
    ("sect" t t nil geex-fontify-element-sect)
    ("file" t t nil geex-fontify-element-file))
  "A list of element specifications for specially highlighting text.
XML-style elements are the best way to add custom highlighting to Freex.
This is easily accomplished by customizing this list of markup elements.

For each entry, the name of the element is given, whether it expects
a closing element and/or an optional set of attributes, whether it is
nestable, and a function that performs whatever action is desired
within the delimited region.

The function is called with three arguments, the beginning and
end of the region surrounded by the elements. If properties are
allowed, they are passed as a third argument in the form of an
alist. The `end' argument to the function is the last character
of the enclosed element or region.

Functions should not modify the contents of the buffer."
  :type '(repeat (list (string :tag "Markup element")
                       (boolean :tag "Expect closing element" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function))
  :group 'geex-fontify)

(defun geex-fontify-buffer ()
  "Re-fontify the entire Freex buffer."
  (interactive)
  (geex-fontify-region (point-min) (point-max) t))

(defvar geex-fontify-regexp-list
  '((geex-element-regexp t
                          geex-xml-process-element
                          (geex-fontify-elements)))
  "List of regexps to search for and process for geex-fontify.")

(defvar geex-fontify-update-implicit-link-regexp-often
  1
  "By default, the implicit links regexp gets updated whenever
new nuggets get added etc. Set this to 0 to update them less
often, because they take a while if you have lots of nuggets.")

(defun geex-fontify-region (beg end &optional verbose)
  ;; (message "geex-fontify-region")
  ;; Next form didn't seem to make a difference if commented out
  ;; remove everything in region, so we can put it back
  ;; do implicit links
  (geex-fontify-process-implicit-links beg end)
  ;; process the region (sect-headings etc.)
  (geex-xml-process-region beg end geex-fontify-regexp-list nil))

(defvar geex-fontify-ignore-implicit-links-to-active-nuggets t
  "If non-nil, fontification will ignore links to nuggets that are
active in the current buffer.")

(defun geex-fontify-update-implicit-link-regexp ()
  (interactive)
  (if (or geex-fontify-update-implicit-link-regexp-often
          (interactive-p))
      (progn
        (message "Updating implicit links regex")
        (geex-sqlalchemy-update-implicit-link-regexp)
        (message "Done"))))

(defun geex-fontify-process-implicit-links (beg end)
  (let ((matches (geex-sqlalchemy-get-all-matching-implicit-links
                  (buffer-substring-no-properties beg end)))
        (modified-p (buffer-modified-p)))
    (message "geex: matches: %s" matches)
    (unwind-protect
        (save-excursion
          (save-restriction
            (widen)
            ;; loop over matches
            (dolist (match matches)
              ;; correct for the offset into the file
              (geex-fontify-implicit-link-from-range (+ beg (nth 0 match))
                                                      (+ beg (nth 1 match))))
            (set-buffer-modified-p modified-p))))))

(defun geex-fontify-implicit-link-from-range (beg end)
  ;; must figure out beg and end
  (let ((link-text (buffer-substring-no-properties beg end))
        (link-file nil))
    ;; only add link if alias exists and not already a link.
    ;;
    ;; PBS - This is why the case-insensitive implicit linking is not
    ;; working.  The alias is returning as not existing because when
    ;; the alias is JubbaWubba and we put in jubbawubba, it's not
    ;; found.
    (when (and (geex-sqlalchemy-exist-nugget-a link-text)
               (not (get-text-property beg 'geex-link)))
      ;; is an alias, so set the link-file from the alias
      (setq link-file (geex-sqlalchemy-get-filename-a link-text))
      ;; set the link-text properties to interactive link
      ;; for most of the range, except the last character,
      ;; which we'll make a non-interactive link so that it
      ;; looks like a link but isn't clickable, making it
      ;; easier to add carriage returns at the end of a link
      (add-text-properties beg
                           (- end 1)
                           (list 'face 'geex-link-face
                                 'geex-link t
                                 'keymap geex-fontify-link-local-map
                                 ;; doesn't seem to help
                                 ;; 'font-lock-multiline t
                                 'link-text link-text
                                 'link-file link-file))
      (add-text-properties (- end 1)
                           end
                           (list 'face 'geex-link-face
                                 'geex-link t
                                 ;; 'font-lock-multiline t
                                 )))))

(defun geex-unhighlight-buffer ()
  (geex-unhighlight-region (point-min) (point-max)))

(defun geex-unhighlight-region (begin end &optional verbose)
  "Remove all visual highlights in the range (except font-lock)."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t)
        (modified-p (buffer-modified-p))
        deactivate-mark)
    (unwind-protect
        (if (eq major-mode 'muse-mode)
            ;; remove only geex-specific properties
            (remove-text-properties
             begin end '(geex-link nil link-file nil link-text nil
                              link-url nil))
          ;; remove all the props
          (remove-text-properties
           begin end '(face nil font-lock-multiline nil end-glyph nil
                            invisible nil intangible nil display nil
                            mouse-face nil keymap nil help-echo nil
                            geex-link nil link-file nil link-text nil
                            link-url nil read-only nil)))
      (set-buffer-modified-p modified-p))))

(defface geex-link-face
  '((((class color) (background light))
     (:foreground "blue" :underline "blue"))
    (((class color) (background dark))
     (:foreground "cyan" :underline "cyan"))
    (t (:bold t)))
  "Face for Freex cross-reference links."
  :group 'geex-fontify)

(defface geex-link-broken-face
  '((((class color) (background light))
     (:foreground "red" :underline "red"))
    (((class color) (background dark))
     (:foreground "red" :underline "red"))
    (t (:bold t)))
  "Face for broken Freex cross-reference links."
  :group 'geex-fontify)

(defface geex-deemphasize-face
'((((class color) (background light))
     (:foreground "DarkGrey"))
    (((class color) (background dark))
     (:foreground "DarkGrey"))
    (t (:bold t)))
  "Face for to deemphasize elements when fontified."
  :group 'geex-fontify)

;; make section faces
(defvar geex-fontify-max-sect 5)
(defun geex-fontify-make-sect-faces (&optional later)
  (dolist (num '(1 2 3 4 5))
    (let ((newsym (intern (concat "geex-sect-" (int-to-string num) "-face"))))
      (eval `(defface ,newsym
                   '((t (:height ,(1+ (* 0.1 (- geex-fontify-max-sect num)))
                                 ;;:inherit variable-pitch
                                 :weight bold)))
                   "Freex section face"
                   :group 'geex-fontify)))))
(progn (geex-fontify-make-sect-faces))

(defun geex-fontify-insert-newline-after-sect ()
  (interactive)
  "Move to the end of the current section element and then insert new line"
  (geex-xml-goto-element-end "sect" nil)
  (insert "\n"))

(defvar geex-fontify-sect-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'geex-fontify-insert-newline-after-sect)
    map)
  "Local keymap used by Freex while on a sect heading.")

(defvar geex-fontify-link-local-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [(control ?m)] 'geex-follow-link-at-point)
    (define-key map [return] 'geex-follow-link-at-point)
    (define-key map [(shift return)] 'geex-follow-link-at-point-other-window)
    ;; (define-key map [(shift return)] 'geex-follow-link-at-point)
    ;; (define-key map [(shift control ?m)] 'geex-follow-link-at-point-other-window)
    ;; (define-key map [(shift control ?m)] 'geex-follow-link-at-point)
    ;; i can't get the mouse-1 stuff to work
    ;; (define-key map [mouse-1] 'geex-follow-link-at-mouse)
    ;; (define-key map [(shift mouse-2)]
    ;;  'geex-follow-link-at-mouse-other-window)
;;     (define-key map [mouse-2] 'geex-follow-link-at-mouse)
;;     (unless (eq emacs-major-version 21)
;;       (set-keymap-parent map geex-mode-map))
    map)
  "Local keymap used by Freex while on a link.")

;; Types of links (generic):
;; <link url="http://jubba.com">Go here</link>
;; <link file="./jubba.txt">Check out this script</link>
;; <link alias="jubba">The jubba file</link>
;; <link id=44>This is nugid 44</link>

;; Other types of links (docbook):
;; <link linkend="id">Must be in same doc.</link>
;; <xref linkend="id"/> Also must be in same doc.

;; <olink targetdoc="alias" targetptr="id">Jubba Doc</olink>
(defun geex-fontify-element-olink (beg end attrs)
  (let ((beg-element-start beg)
        (beg-element-finish nil)
        (end-element-start nil)
        (end-element-finish end)
        (targetdoc (cdr (assoc "targetdoc" attrs)))
        (targetptr (cdr (assoc "targetptr" attrs)))
        (link-text nil)
        (link-file nil)
        (current-font-face 'geex-link-face))
    ;; set the missing parts of the element locations
    (save-match-data
      (goto-char beg)
      (setq beg-element-finish (and (looking-at "<[^>]+>")
                                 (match-end 0)))
      (goto-char end)
      (setq end-element-start (and (geex-looking-back "</[^>]+>")
                                   (match-beginning 0))))
    ;; see if it matches an alias
    (if (geex-sqlalchemy-exist-nugget-a targetdoc)
        ;; is an alias, so set the link-file from the alias
        (setq link-file (geex-sqlalchemy-get-filename-a targetdoc))
      ;; process as a file
      (setq link-file targetdoc))
    ;; make sure file exists, if not, set link-file to nil
    ;; XXX MUST DO THIS
    (when (not link-file)
      ;; change the font face to broken link
      (setq current-font-face 'geex-link-broken-face))
    ;; set the link-text
    (setq link-text (buffer-substring-no-properties
                     beg-element-finish
                     end-element-start))
    ;; set the link-text properties
    (add-text-properties beg
                         end
                         (list 'face current-font-face
                               'geex-link t
                               'keymap geex-fontify-link-local-map
                               ;;'font-lock-multiline t
                               'link-text link-text
                               'link-file link-file))
    ;; hide the element part
    (add-text-properties beg-element-start
                         beg-element-finish
                         (list 'invisible t))
    (add-text-properties end-element-start
                         end-element-finish
                         (list 'invisible t))))

;; <ulink url="http://jubba.html>My jubba site</ulink>
(defun geex-fontify-element-ulink (beg end attrs)
  (let ((beg-element-start beg)
        (beg-element-finish nil)
        (end-element-start nil)
        (end-element-finish end)
        (link-url (cdr (assoc "url" attrs)))
        (link-text nil)
        (link-file nil)
        (current-font-face 'geex-link-face))
    ;; set the missing parts of the element locations
    (save-match-data
      (goto-char beg)
      (setq beg-element-finish (and (looking-at "<[^>]+>")
                                 (match-end 0)))
      (goto-char end)
      (setq end-element-start (and (geex-looking-back "</[^>]+>")
                                   (match-beginning 0))))
    ;; set the link-text
    (setq link-text (buffer-substring-no-properties
                     beg-element-finish
                     end-element-start))
    ;; set the link-text properties
    (add-text-properties beg
                         end
                         (list 'face current-font-face
                               'geex-link t
                               'keymap geex-fontify-link-local-map
                               ;;'font-lock-multiline t
                               'link-text link-text
                               'link-url link-url))
    ;; hide the element part
    (add-text-properties beg-element-start
                         beg-element-finish
                         (list 'invisible t))
    (add-text-properties end-element-start
                         end-element-finish
                         (list 'invisible t))))

(defvar geex-fontify-sect-number-format "%d) ")
(defun geex-fontify-element-sect (beg end attrs)
  (let ((beg-element-start beg)
        (beg-element-finish nil)
        (end-element-start nil)
        (end-element-finish end)
        (sect-level (cdr (assoc "level" attrs)))
        (sect-text nil)
        (sect-face nil))
    ;; set the missing parts of the element locations
    (save-match-data
      (goto-char beg)
      (setq beg-element-finish (and (looking-at "<[^>]+>")
                                 (match-end 0)))
      (goto-char end)
      (setq end-element-start (and (geex-looking-back "</[^>]+>")
                                   (match-beginning 0))))
    ;; set the sect-text
    (setq sect-text (buffer-substring-no-properties
                     beg-element-finish
                     end-element-start))
    ;; determine the proper face
    (when (and (> (string-to-number sect-level) 0)
               (< (string-to-number sect-level) geex-fontify-max-sect))
      ;; set the face
      (setq sect-face (intern (concat "geex-sect-" sect-level "-face"))))
    ;; set the sect-text properties
    (add-text-properties beg
                         end
                         (list 'face sect-face
                               'keymap geex-fontify-sect-local-map
                               'font-lock-multiline t))
    ;; hide the element part
    ;; Trying out adding numbering
    ;; It breaks the use of xml-parse
    ;;     (add-text-properties beg-element-start
    ;;       beg-element-finish
    ;;       (list 'display
    ;;             (format geex-fontify-sect-number-format
    ;;               (string-to-number sect-level))
    ;;             'intangible t))
    (add-text-properties beg-element-start
                         beg-element-finish
                         (list ;'face 'geex-deemphasize-face
                               'invisible t
                               'intangible t
                               ;'read-only t
                               'rear-nonsticky t))
    (add-text-properties end-element-start
                         end-element-finish
                         (list ;'face 'geex-deemphasize-face
                               'invisible t
                               'intangible t
                               ;'read-only t
                               'rear-nonsticky t))))

(defun geex-fontify-insert-sect-element (level)
  "Insert a section element at point."
  (interactive "nEnter section level (1-5): ")
  (let ((attrs (format "level=\"%d\"" level))
        (sect-txt ""))
    ;; see if there is marked text to use instead
    (save-excursion
      (when mark-active
        ;; delete and set the text
        (setq sect-txt (delete-and-extract-region (point) (mark t))))
      ;; insert the element
      (geex-xml-insert-element-at-point "sect" attrs sect-txt
                                         'geex-fontify-region)
      ;; move the point to ideal place to enter the element text
      (goto-char (search-backward "</sect>" nil t)))))

(defun geex-fontify-insert-sect-element-1 ()
  (interactive)
  (geex-fontify-insert-sect-element 1))

(defun geex-fontify-insert-sect-element-2 ()
  (interactive)
  (geex-fontify-insert-sect-element 2))

(defun geex-fontify-insert-sect-element-3 ()
  (interactive)
  (geex-fontify-insert-sect-element 3))

(defun geex-fontify-insert-sect-element-4 ()
  (interactive)
  (geex-fontify-insert-sect-element 4))

;; (defun geex-fontify-decrease-sect-level ()
;;   (geex-xml-replace-element-at-point element attrs postfun))

;; define how to handle an embedded file
(defun geex-fontify-element-file (beg end attr)
    (let ((beg-file nil)
          (end-file nil)
          (filename nil)
          (text-tween-elements nil))
      (save-match-data
        (goto-char beg)
        (setq beg-file (and (looking-at "<[^>]+>")
                            (match-end 0)))
        (goto-char end)
        (setq end-file (and (geex-looking-back "</[^>]+>")
                            (match-beginning 0))))
      ;; get the file info
      (setq text-tween-elements (buffer-substring-no-properties
                             beg-file
                             end-file))
      ;; feed the fileroot.geex in to the insert-file
      ;; functions
      (setq filename (geex-remove-extension text-tween-elements))
      (when (not (file-exists-p filename))
        ;; tell them that the file does not exist
        (add-text-properties beg-file
                             end-file
                             (list 'face 'geex-link-broken-face)))))

(provide 'geex-fontify)
