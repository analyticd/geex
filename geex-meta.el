;;; geex-meta.el --- coloring and highlighting used by Freex
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
(require 'geex-embed)

;; you'll need lines that look like this in your .emacs
;;
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (eval-after-load "pymacs"
;;   '(add-to-list 'pymacs-load-path "/home/greg/elisp/geex"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; append our custom element to geex-embed-elements
(add-to-list 'geex-embed-elements
             '("meta" t t nil geex-meta-create-meta))

(add-to-list 'geex-embed-elements
             '("tag-parents" t t nil
               geex-meta-create-tag-parents))

(add-to-list 'geex-embed-elements
             '("tag-children" t t nil
               geex-meta-create-tag-children))

(add-to-list 'geex-embed-elements
             '("aliases" t t nil geex-meta-create-aliases))

(add-to-list 'geex-embed-elements
             '("embedded-tag-children" t t nil geex-meta-create-embedded-tag-children))

(defvar geex-meta-complete-alias-hist nil
  "This is the history list for geex-meta-complete-alias." )

(defvar geex-meta t
  "When this variable exists, it shows that geex-meta has
been loaded and that the functions are available for use.")

(defvar geex-db-name "geex.db"
  "A relative path to the database file from geex-mode-dir.")

;; the '-hooks' is supposed to signify that these
;; take a nugget alias as an argument - see
;; http://www.gnu.org/software/emacs/elisp/html_node/Hooks.html
(defvar geex-meta-add-nugget-hooks nil
  "A list of hooks that take an ALIAS argument that will get run
whenever geex-meta-update-index creates a new nugget.")

(defvar geex-mode-ext "geex"
  "The extension for all geex files. Should not include a dot.")

(defun geex-full-db-name ()
  "Returns the full pathname to the database, by joining
geex-mode-dir and geex-db-name. It may be possible just to
make this a variable, but it's important that it gets
updated somehow *after* the user has set those variables in
their .emacs."
  ;; couldn't access lisp variables, so turned
  ;; geex_full_db_name into a function
  (concat (file-name-as-directory geex-mode-dir)
          geex-db-name))

(defun geex-meta-connect-init-db ()
  "This will try and connect to the db. If the db doesn't exist,
it will create it. Returns the Fsqa instance from
geex-sqlalchemy."
  (pymacs-load "geex_sqlalchemy")
  (geex-sqlalchemy-create-fsqa
   (geex-full-db-name) ; db file
   1 ; use_lisp
   0 ; in_memory
   geex-mode-ext ; file extension
   ))

(defvar fsqa nil
"This is the global instance of the Fsqa class that contains all
the db and metadata objects in geex-sqlalchemy.")
;; use a setq rather than setting fsqa directly in the
;; defvar, because defvar will not overwrite an existing
;; non-nil variable
(setq fsqa (geex-meta-connect-init-db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun geex-meta-create-meta (beg end id)
  "Handle an embedded metadata."
  (let ((beg-file nil)
        (end-file nil)
        (priority
         (+ (geex-embed-get-highest-priority-at beg) 1))
        (ov nil)
        (filename (geex-sqlalchemy-get-filename id))
        )
    (save-match-data
      (goto-char beg)
      (setq beg-file (and (looking-at "<[^>]+>")
                          (match-end 0)))
      (goto-char end)
      (setq end-file (and (geex-looking-back "</[^>]+>")
                          (match-beginning 0))))
    ;; insert metadata as overlay
    (setq ov
          (geex-embed-create-overlay beg end priority
                                      'geex-meta-insert-meta
                                      'geex-embed-overlay-save-null
                                      (list
                                       'id id
                                       'filename filename)))
    (geex-embed-region (overlay-start ov) (overlay-end ov))
    ;; goto the beginning of the region
    (goto-char beg)
    ))

(defun geex-meta-insert-meta (ov)

  (let ((filename (overlay-get ov 'filename))
        (id (overlay-get ov 'id)))

    ;; propertize sets the text properties to intangible
    (insert (propertize
             (concat "filename: " filename "\n\n")
             'intangible nil))

    ;; propertize sets the text properties to intangible
    (insert (propertize
             (concat "id: " id "\n\n")
             'intangible nil))

    (insert (concat (propertize "aliases: " 'intangible nil)
                    "<aliases>" id "</aliases>"
                    (propertize "\n\n" 'intangible nil)))

    (insert (concat (propertize "tag-parents: " 'intangible nil)
                    "<tag-parents>" id "</tag-parents>"
                    (propertize "\n\n" 'intangible nil)))

    (insert (concat (propertize "tag-children: " 'intangible nil)
                    "<tag-children>" id "</tag-children>"
                    (propertize "\n\n" 'intangible nil)))

    (insert (propertize
             (format "modtime: %s \n\n"
                     (geex-sqlalchemy-get-last-modtime id))))

    (overlay-put ov 'meta t)))

(defun geex-meta-insert-metadata-here ()
  "Create a throwaway metadata overlay at point. By default,
this will be metadata for the buffer/embedding at point."
  (interactive)
  ;; get the nugid from the filename

  (let ((id (geex-embed-get-id-at-point)))

    ;; before going any further, find any existing meta
    ;; overlays for this nug and close them to avoid the
    ;; possibility of editing the metadata differently in
    ;; different places
    (geex-meta-remove-meta-overlays id)

    (geex-meta-create-meta (point) (point) id)))

(defun geex-meta-get-meta-overlays (&optional nugid)
  "Returns a list containing meta overlays in this buffer. If
NUGID (string) is nil, then it returns all the meta
overlays. Otherwise, it'll just return the meta overlays that
belong to NUGID.

N.B. determines whether an overlay is a metadata overlay by
checking whether it uses geex-meta-insert-meta as the
insert-funct - in the future, it might be necessary to define a
special is-geex-meta overlay property, in case there are meta
overlays that use different insert-functs."
  (when (numberp nugid)
    (setq nugid (number-to-string nugid)))

  (let* (
         ;; list of properties that the overlay must match
         ;;
         ;; at the very least, they must have
         ;; 'geex-meta-insert-meta as their insert-funct
         (props
          (if nugid
              ;; if the user specified a nugid, then we need
              ;; to filter down to just the meta overlays
              ;; belonging to that NUGID
              (list
               (list 'insert-funct 'geex-meta-insert-meta)
               (list 'id nugid))
            ;; if the user didn't specify a NUGID, then we
            ;; can just return all the meta overlays
            (list
             (list 'insert-funct 'geex-meta-insert-meta)))))

    (geex-embed-overlays-with-properties-in
     (point-min) (point-max) props)))

(defun geex-meta-remove-meta-overlays (&optional nugid)
  "Remove all the meta overlays for nugid NUGID. If NUGID is nil,
it will remove all meta-overlays in the buffer."

  (let ((meta-ovs nil))
    (setq meta-ovs (geex-meta-get-meta-overlays nugid))

    ;; In principle, there should be no way that more than
    ;; one meta overlay for any nugget can be open in the
    ;; same buffer at once, because geex-meta-insert-meta
    ;; automatically removes any existing meta overlays
    ;; before inserting a new one. However, just in case,
    ;; this function allows for the possibility that there
    ;; *are* more than meta overlays open for NUGID, and so
    ;; it loops over all that it finds. Since it's at least
    ;; possible that these meta overlays might be nested
    ;; within each other, removing the outer one would
    ;; remove all those within, and then there would be
    ;; fewer than the list, so it updates its list each time
    ;; it removes one."
    (while meta-ovs
      (geex-embed-remove-overlay (car meta-ovs) t)
      (setq meta-ovs (geex-meta-get-meta-overlays nugid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag parents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun geex-meta-create-tag-parents (beg end attr)
  (let ((beg-file nil)
        (end-file nil)
        (id (geex-embed-get-id-at-point))
        (priority
         (+ (geex-embed-get-highest-priority-at beg) 1)))
    (save-match-data
      (goto-char beg)
      (setq beg-file (and (looking-at "<[^>]+>")
                          (match-end 0)))
      (goto-char end)
      (setq end-file (and (geex-looking-back "</[^>]+>")
                          (match-beginning 0))))
    ;; insert metadata as overlay
    (geex-embed-create-overlay beg end priority
                                'geex-meta-insert-tag-parents
                                'geex-meta-save-tag-parents
                                (list 'id id))
    ;; goto the beginning of the region
    (goto-char beg)))

(defun geex-meta-insert-tag-parents (ov)
  (insert (concat (geex-sqlalchemy-get-tag-parents-delim
                   (overlay-get ov 'id)) " "))
  ov)

(defun geex-meta-save-tag-parents ()
  (geex-sqlalchemy-put-tag-parents-delim
   (plist-get geex-embed-ov-props 'id) (buffer-string))
  ;; (set-buffer-modified-p nil)
  t)

(defun geex-meta-edit-tag-parents-in-minibuffer ()
  "Presents the semicolon-delimited list of tag-parents for
editing in the minibuffer. See also:
geex-meta-minibuffer-aliases."
  (interactive)
  (let* ((id (or (geex-embed-get-id-at-point)
                 ;; this nugget doesn't exist. save it first so that it
                 ;; exists in the database before we try to add tags
                 (progn
                   (save-buffer)
                   (setq id (geex-embed-get-id-at-point)))))
         (tag-parents (geex-sqlalchemy-get-tag-parents-delim-slash id)))

    ;; allows nuggets with spaces in. see complete-alias for
    ;; more info
    (define-key minibuffer-local-completion-map (kbd "SPC") nil)

    ;; replace the semicolon-delimited tag-parents list with the
    ;; input from the minibuffer (seeded with the current list
    ;; of tag-parents
    (message
     (geex-sqlalchemy-put-tag-parents-delim-slash
      id
      (completing-read
       "Tag-parents: "
       'geex-meta-edit-tag-parents-in-minibuffer-dct
       nil ; predicate
       nil ; require match
       ;; apparently, initial input has been deprecated, but it's useful...
       tag-parents ; initial input
       geex-meta-complete-alias-hist
       ))
   )))

;; we don't need to write this out explicitly as a function,
;; but it makes it easier to debug
;;
;; (insert (format "\n\n%s"
;;                 (dynamic-completion-table
;;                  geex-sqlalchemy-edit-tag-parents-in-minibuffer-complete)))
(defun geex-meta-edit-tag-parents-in-minibuffer-dct (string predicate mode)
  (with-current-buffer (let ((window (minibuffer-selected-window))) (if (window-live-p window) (window-buffer window) (current-buffer))) (cond ((eq mode t) (all-completions string (geex-sqlalchemy-edit-tag-parents-in-minibuffer-complete string) predicate)) ((not mode) (try-completion string (geex-sqlalchemy-edit-tag-parents-in-minibuffer-complete string) predicate)) (t (test-completion string (geex-sqlalchemy-edit-tag-parents-in-minibuffer-complete string) predicate)))))

(defun geex-meta-add-tag-parents-in-minibuffer ()
  "Keeps asking you for tag-parents (with tab completion) and
inserting them until you give it a blank one or press C-g."
  (interactive)
  (let* (
         ;; the child nugget we're going to add tag-parents
         ;; to
         (child-nugid (geex-embed-get-id-at-point))
         ;; the user's response to geex-meta-complete-alias
         (chosen nil)
         ;; the child nugget's existing tag-parents
         (tag-parents nil)
         ;; the tag-parent we're about to add
         (new-tag-parent nil))

    ;; allows nuggets with spaces in. see complete-alias for
    ;; more info
    (define-key minibuffer-local-completion-map (kbd "SPC") nil)

    ;; definitely ask for at least one tag-parent. if it's
    ;; blank, add-tag-parent will just shrug and return
    ;; nil. if it's non-blank and legit, add-tag-parent will
    ;; return the tag-parent back
    (setq chosen
          (geex-meta-complete-alias nil "New tag-parent: "))
    (setq new-tag-parent
          (geex-sqlalchemy-add-tag-child-to-tag-parent-a
           child-nugid
           chosen))

    ;; and as long as the user keeps providing non-blank
    ;; legitimate tag-parents, keep on askin'
    (while (> (length chosen) 0)
      (setq chosen
            (geex-meta-complete-alias nil "New tag-parent: "))
      (setq new-tag-parent
            (geex-sqlalchemy-add-tag-child-to-tag-parent-a
             child-nugid
             chosen)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tag-children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun geex-meta-create-tag-children (beg end attr)
  (let ((beg-file nil)
        (end-file nil)
        (id (geex-embed-get-id-at-point))
        (priority
         (+ (geex-embed-get-highest-priority-at beg) 1)))
    (save-match-data
      (goto-char beg)
      (setq beg-file (and (looking-at "<[^>]+>")
                          (match-end 0)))
      (goto-char end)
      (setq end-file (and (geex-looking-back "</[^>]+>")
                          (match-beginning 0))))
    ;; insert metadata as overlay
    (geex-embed-create-overlay beg end priority
                                'geex-meta-insert-tag-children
                                'geex-meta-save-tag-children
                                (list 'id id))
    ;; goto the beginning of the region
    (goto-char beg)))

(defun geex-meta-insert-tag-children (ov)
  (insert (concat (geex-sqlalchemy-get-tag-children-delim
                   (overlay-get ov 'id)) " "))
  ov)

(defun geex-meta-save-tag-children ()
  (geex-sqlalchemy-put-tag-children-delim
   (plist-get geex-embed-ov-props 'id) (buffer-string))
  ;; (set-buffer-modified-p nil)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun geex-meta-create-aliases (beg end &rest ignore)
  (let ((beg-file nil)
        (end-file nil)
        (id (geex-embed-get-id-at-point))
        (priority
         (+ (geex-embed-get-highest-priority-at beg) 1)))
    (save-match-data
      (goto-char beg)
      (setq beg-file (and (looking-at "<[^>]+>")
                          (match-end 0)))
      (goto-char end)
      (setq end-file (and (geex-looking-back "</[^>]+>")
                          (match-beginning 0))))
    ;; insert metadata as overlay
    (geex-embed-create-overlay beg end priority
                                'geex-meta-insert-aliases
                                'geex-meta-save-aliases
                                (list 'id id))
    ;; goto the beginning of the region
    (goto-char beg)))

(defun geex-meta-insert-aliases (ov)
  (insert (concat (geex-sqlalchemy-get-aliases-delim
                    (overlay-get ov 'id)) " "))
  ov)

(defun geex-meta-save-aliases ()
  (geex-sqlalchemy-put-aliases-delim
   (plist-get geex-embed-ov-props 'id) (buffer-string))
  ;; (set-buffer-modified-p nil)
  t)

(defun geex-meta-edit-aliases-in-minibuffer ()
  "Presents the semicolon-delimited list of aliases for editing
in the minibuffer."
  (interactive)
  (let* ((id (geex-embed-get-id-at-point))
         (aliases (geex-sqlalchemy-get-aliases-delim id)))
  ;; replace the semicolon-delimited aliases list with the
  ;; input from the minibuffer (seeded with the current list
  ;; of aliases
  (geex-sqlalchemy-put-aliases-delim
   id
   (read-string ;; (prompt initial-input)
    "Aliases: " aliases)))
  (geex-fontify-update-implicit-link-regexp))

(defun geex-meta-add-aliases-in-minibuffer ()
  "Keeps asking you for aliases (with tab completion) and
inserting them until you give it a blank one or press C-g."
  (interactive)
  (let* ((id (geex-embed-get-id-at-point))
         (new-alias nil))

    ;; definitely ask for at least one alias. if it's blank,
    ;; add-alias will just shrug and return nil. if it's
    ;; non-blank and legit, add-alias will return the alias
    ;; back
    (setq new-alias
          (geex-sqlalchemy-add-alias
           id
           (geex-meta-complete-alias "New alias: " nil)))
    ;; and as long as the user keeps providing non-blank
    ;; legitimate aliases, keep on askin'
    (while (> (length new-alias) 0)
      (setq new-alias
            (geex-sqlalchemy-add-alias
             id
             (geex-meta-complete-alias nil "New alias: "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; embedded-tag-children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; based on geex-embed-file-element,
;; geex-embed-insert-file and geex-embed-save-file
;; functions
;;
;; embeds all the tag-children of some set of tag parents in
;; the current document. very useful for scanning all your
;; notes about a given topic

(defun geex-meta-create-embedded-tag-children (beg end attr)
    ;(geex-unhighlight-region beg end)
    (let ((beg-file nil)
          (end-file nil)
          (tag-parents-str nil)
          (priority (+ (geex-embed-get-highest-priority-at beg) 1)))

      (save-match-data
        (goto-char beg)
        (setq beg-file (and (looking-at "<[^>]+>")
                            (match-end 0)))
        (goto-char end)
        (setq end-file (and (geex-looking-back "</[^>]+>")
                            (match-beginning 0))))

      ;; the text between the elements tells us the
      ;; tag-parents whose children we're going to embed
      (setq tag-parents-str (buffer-substring-no-properties
                             beg-file
                             end-file))

      ;; create the new overlay that will contain all our
      ;; embedded tag children
      (geex-embed-create-overlay beg end priority
                                  'geex-meta-insert-embedded-tag-children
                                  'geex-meta-save-embedded-tag-children
                                  (list 'tag-parents-str tag-parents-str))
      (goto-char beg)))

(defun geex-meta-insert-embedded-tag-children (ov)

  (let* ((tag-parents-str (overlay-get ov 'tag-parents-str))
         (tag-parents-lst
          ;; get the list of nuggets to embed (without their
          ;; tag-parents preamble)
          (geex-sqlalchemy-filter-by-tag-parents-fnames-only
           tag-parents-str)))

    (unless (> (length tag-parents-str) 0)
      (error "Can't embed all the nuggets in the database - you have to specify a tag-parents-str when embedding tag-children"))

    ;; put a carriage return at the top, before all the
    ;; embedded children come to play
    (insert "\n")

    ;; insert all the tag children here
    (insert (geex-meta-create-embedroll tag-parents-lst))

    ;; and a couple at the end
    (insert "\n\n")

    ;; and store it for good measure
    (overlay-put ov 'tag-parents-lst tag-parents-lst))
  ov)

(defun geex-meta-save-embedded-tag-children ()
  ;; not sure if i have to do anything here, so i'm leaving
  ;; it blank
  t)

(defun geex-meta-insert-embedded-tag-children-in-temp-buffer (tag-parents-str)

  (let* ((bufname (format "embed %s" tag-parents-str))
         ;; swap the slashes for hyphens, and get rid of the stars
         (vis-filename
          (geex-meta-replace-regexp-in-string
           "/" "-"
           (geex-meta-replace-regexp-in-string
            "*" "" bufname))))

    ;; generate a temp buffer and switch to it
    (switch-to-buffer
     (generate-new-buffer bufname))

    ;; set the buffer-file-name for this buffer, so that
    ;; it's not a scratch buffer any more. this will allow
    ;; us to save without it asking us for a file
    ;; name. N.B. this file won't actually get created at
    ;; any point

    (set-visited-file-name
     (concat geex-mode-dir vis-filename
             "." geex-mode-ext))

    (setq geex-embed-ov-props
          (plist-put geex-embed-ov-props 'save-funct
                     'geex-embed-overlay-save-null))

    ;; we're wrapping this in a condition-case (i.e. a
    ;; try/catch, in case there *are* no embedded
    ;; tag-children)
    ;;
    ;; it might be simpler to just count the number of
    ;; tag-children with a db call...
    (condition-case nil
        (geex-xml-insert-element-at-point "embedded-tag-children"
                                           nil tag-parents-str
                                           'geex-embed-region)
      (error nil))

    (set-buffer-modified-p nil)))

(defun geex-meta-create-embedroll (nuglist)
  "Inserts an embedroll (a list of <file>embedme</file> calls,
with a pair of lines between each. Does not append an extension
to the items in NUGLIST."
  (geex-join
   (mapcar
    (lambda (nug) (format "<file>%s</file>" nug))
    nuglist) "\n\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun geex-meta-member-a-not-in-b (lst-a lst-b)
   "Return a list of the elements in lst-a that are not in
lst-b."
  (let ((a-not-in-b nil))
    (dolist (el-a lst-a)
      (when (not (member el-a lst-b))
        (push el-a a-not-in-b)))
    a-not-in-b))

(defun geex-meta-remove-regexp-from-string (regexp str)
  "Removes all instances of the regexp from the string."
  (geex-meta-replace-regexp-in-string regexp "" str))

;; from muse-replace-regexp-in-string
(defun geex-meta-replace-regexp-in-string
  (regexp replacement text &optional fixedcase literal)
  "Replace REGEXP with REPLACEMENT in TEXT.

Return a new string containing the replacements.

If fourth arg FIXEDCASE is non-nil, do not alter case of replacement text.
If fifth arg LITERAL is non-nil, insert REPLACEMENT literally."
  (cond
   ((fboundp 'replace-in-string)
    (replace-in-string text regexp replacement))
   ((fboundp 'replace-regexp-in-string)
    (replace-regexp-in-string regexp replacement text fixedcase literal))
   (t (let ((repl-len (length replacement))
            start)
        (save-match-data
          (while (setq start (string-match regexp text start))
            (setq start (+ start repl-len)
                  text (replace-match replacement fixedcase literal text)))))
      text)))

;; (defun geex-meta-rename-this ()
;;   "Updates the filename and database alias for the nugget at
;; point. xxx - does not run a find and replace to update references
;; to or embeddings of this nugget."
;;   (interactive)
;;   (let* ((id (geex-embed-get-id-at-point))
;;          (old-fname (geex-sqlalchemy-get-filename id))
;;          (aliases (geex-sqlalchemy-get-all-filenames))
;;          (new-fname nil))

;;     ;; xxx - extracted from the renaming bit of
;;     ;; geex-meta-update-index
;;     (completing-read
;;      (format "Update filename for '%s' to: " old-fname)
;;      aliases ;; provide the existing filenames to help construct the new filename
;;      nil ;; predicate
;;      nil ;; require-match
;;      nil ;; initial-input
;;      ))

;;   ;; save
;;   ;;
;;   ;; don't allow them to rename to an existing file
;;   ;;
;;   ;; make sure it has the extension on the end
;;   ;;
;;   ;; rename the file. check that worked
;;   ;;
;;   ;; rename the db
;;   ;;
;;   ;; kill the buffer, then reopen it
;; )

(defun geex-meta-update-index ()
  "Update the metadata index in the db according to the
files in the directory.

1) Find all the files without db entries.

2) Find all the db entries without files.

3) Deal with #2 by allowing you to redirect or delete those
db entries.

4) If a file now has a db entry associated with it,
take it off the list of 'files with no db entries'.

5) Any of the 'files without db entries' remaining must be
brand new files that have never yet existed in the
db. Create db entries for them.
"
  (interactive)

  (let ((full-db-name (geex-full-db-name)))

    ;; if the db file doesn't exist, create it
    (when (not (file-exists-p full-db-name))
      (message (format "Creating %s" full-db-name))
      (geex-meta-connect-init-db))

    (let ((files (directory-files
                  geex-mode-dir ;; where to look
                  nil ;; full file names?
                  geex-mode-dir-filter ;; file match regex
                  nil)) ;; nosort
          ;; (db-aliases (geex-sqlalchemy-get-all-aliases))
          (db-filenames (geex-sqlalchemy-get-all-filenames))
          (db-but-no-file nil)
          (file-but-no-db nil)
          (file-but-no-db-alist nil))

      ;; find all the files that don't have db entries - step
      ;; #1
      (setq file-but-no-db
            (geex-meta-member-a-not-in-b files db-filenames))

      ;; find all the db entries whose filenames don't exist
      ;; (because they've either been moved or renamed) - step
      ;; #2
      (setq db-but-no-file
            (geex-meta-member-a-not-in-b db-filenames files))

      ;; ask the user how to deal with those db entries that
      ;; don't have files - step #3
      (dolist (dbnf db-but-no-file)
        ;; sanity check
        (when (equal (length dbnf) 0)
          (error "empty filename"))
        ;; give the user the option of what to do with db
        ;; entries whose file doesn't exist
        (setq file-but-no-db-alist
              (mapcar 'list file-but-no-db))
        (setq new-filename
              ;; There is a nugget in the database registered
              ;; to filename %s, but no actual file. Leave
              ;; this field blank to remove the nugget, or
              ;; specify a new filename that it should be
              ;; registered to:
              ;;
              ;; make each one element of file-but-no-db into
              ;; a list, cos that's how completing-read likes
              ;; it
              (completing-read
               (format "Update filename for '%s' to (enter ! to remove nugget): " dbnf)
               (push
                '("!") ;; add "!" as a possible completion
                file-but-no-db-alist)
               nil ;; predicate
               t ;; require-match
               nil ;; initial-input
               ))
        (if (equal new-filename "!")
            ;; (equal (length new-filename) 1)
            ;; if they left the field blank, confirm they want
            ;; to delete the db record
            (if (yes-or-no-p
                 (format "Are you sure you want to delete '%s'" dbnf))
                ;; need to delete all the nuggets that have
                ;; dbnf as a filename
                (geex-sqlalchemy-remove-nugget
                 (geex-sqlalchemy-get-nugid-from-filename dbnf))
              (error "Then what *do* you want to do?"))
          ;; if they gave us a filename to use, change the
          ;; filename to that
          (progn
            (geex-sqlalchemy-change-filename-from dbnf new-filename)
            ;; don't allow the user to complete to this file
            ;; any more - step #4
            (setq file-but-no-db
                  (delete new-filename file-but-no-db))
            (message
             (format "Changing %s to %s" dbnf new-filename)))))

      ;; for every file without a db entry, create a db entry
      ;; - step #5
      (dolist (fbnd file-but-no-db)

        (let ((fbnd-no-ext (geex-sqlalchemy-remove-ext fbnd)))

          ;; if we're mirroring the content in the db, this has
          ;; to change
          ;;
          ;; add a nugget with alias = fbnd (no extension), and
          ;; with filename = fbnd
          ;;
          ;; xxx - i'm pretty sure that we could get away
          ;; with just this, and so we don't need
          ;; add-nugget-called - 070326
          (geex-sqlalchemy-add-nugget fbnd "")

          ;; in case there are any functions that the user
          ;; wants to run whenever a new nugget gets created
          (dolist (hook geex-meta-add-nugget-hooks)
            (eval (list hook fbnd-no-ext)))

        (message (format "Added %s to db" fbnd))))))

  (message "Finished checking files")

  (geex-sqlalchemy-update-implicit-link-regexp)
  (message "Finished updating regex")
  (message nil))

(defun geex-meta-remove-parents-from-alias-chosen (chosen)
  "geex-sqlalchemy-filter-by-tag-parents returns an alias, filtered by
  a \-delimited list of tag-parents. For this completion to work,
  it has to return a string that includes that \-delimited list,
  e.g. 'boosting/lecture/yourstubhere'. We only want the final
  alias chosen, e.g. 'yourstubhere'. So this will get rid of
  everything up to that final '/'."
  (geex-meta-replace-regexp-in-string
   "\\(.*\\)/" "" chosen))

(defun geex-meta-parse-alias-into-tag-parents (alias)
  "By default, new nuggets enter the world with no tag-parents.
However, if you want them to be seeded with tag-parents by
parsing their own alias, then add this function to the
geex-meta-add-nugget-hooks hook in your .emacs file, e.g.

 (add-hook 'geex-meta-add-nugget-hooks
          'geex-meta-parse-alias-into-tag-parents)

Then, it will automatically look at the alias as a list
delimited by \" - \" (a hyphen with a space either side), and
insert each component as a tag-parent.

Currently, this doesn't allow optional arguments if you'd like to
specify all the delimiters yourself, because hyphen seemed a
reasonable choice.

If there are no hyphens in the alias name, then no tag-parents
will be added.

If there are already existing tag-parents, this will not remove
them.
"
  ;; get the existing tag-parents
  (let ((tag-parents
         (geex-sqlalchemy-get-tag-parents-delim-a alias)))

    ;; create a new, longer, semicolon-delimited list with
    ;; both previous and new tag-parents
    ;;
    ;; it doesn't matter if there are duplicates or blanks -
    ;; put-tag-parents-delim will deal with all that
    (setq tag-parents
          (concat tag-parents
                  " ; "
                  (geex-meta-hyphens-to-semicolons-delim alias)))

  ;; set ALIAS's tag-parents to our new list of tag-parents
  (geex-sqlalchemy-put-tag-parents-delim-a
   alias
   tag-parents)))

;; (geex-join '("1" "2" "3") ";")
;; should return "1;2;3"
(defun geex-join (lst delim)
  "Like python's string.join, takes in a list of strings,
and a string delimiter, and inserts the delimiter in between
every element. Doesn't do much error-checking, so make sure
you send in a list of strings."
  (let ((str (car lst)))
    (setq lst (cdr lst))
    (dolist (el lst)
      (setq str (concat str delim el)))
    str))

;; "lecture - blah- boosting" -> "lecture ; blah- boosting"
(defun geex-meta-hyphens-to-semicolons-delim (str)
  (geex-meta-replace-regexp-in-string " - " " ; " str))

(defun geex-meta-define-new-or-insert-metadata ()
  "If the mark is active, then
geex-embed-define-region-as-nugget and open up metadata for
it. If the mark is inactive, then just open up metadata for the
highest overlay at point."
  (interactive)

  ;; this nugget doesn't exist. save it first so that it exists in
  ;; the database before we try and deal with it
  (let ((id (geex-embed-get-id-at-point)))
    (unless id
      (progn
        (save-buffer)
        (setq id (geex-embed-get-id-at-point)))))

  (if mark-active
      (geex-embed-define-region-as-nugget (point) (mark))
    (geex-meta-insert-metadata-here)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completion functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun geex-meta-get-query (&optional prompt require-match in-in)

  "Returns a query like 'emacs//*' or 'loci1/journal -
070906'. Doesn't actually run that query to see what it returns."

  ;; store the current state of partial-completion-mode,
  ;; because we're going to turn it off for the duration of
  ;; this function
  (let (
        ;; we're shadowing the real
        ;; minibuffer-local-completion-map with this local
        ;; version. at the end of the let block, the real
        ;; one will take over again
        (mlcm minibuffer-local-completion-map)
        (query nil))

    ;; this is only going to affect the local version. it'll
    ;; stop SPC being bound to complete, so that we can type
    ;; new nuggets with spaces in
    (define-key minibuffer-local-completion-map (kbd "SPC") nil)
    ;; xxx - this really ought to be a configurable option -
    ;; without this, M-v is bound to pagedown...
    (define-key minibuffer-local-completion-map "\M-v" 'yank)

    (setq query
          (completing-read
           ;; if they fed in a prompt, use that, else default
           (if prompt prompt "Freex alias: ") ;; prompt

           ;; programmatically complete, whittling down by
           ;; tag-parents
           'geex-meta-filter-by-tag-parents-dct

           nil ;; predicate
           require-match
           in-in ;; initial-input
           geex-meta-complete-alias-hist))

    ;; set the keymap back to whatever it was
    (setq minibuffer-local-completion-map mlcm)

    query))

;; xxx at some point, i should rename get-query, or point all
;; the complete-alias calls to get-query
(defun geex-meta-complete-alias (&optional prompt require-match in-in)
  (geex-meta-remove-parents-from-alias-chosen
   (geex-meta-get-query prompt require-match in-in)))

(defun geex-meta-new-file-header (filename_noext)
  (insert (concat "<sect level=\"1\">" filename_noext "</sect>\n\n")))

(defun geex-meta-new-muse-file-header (filename_noext)
  (insert (concat "#title " filename_noext "\n\n")))

(defun geex-meta-find ()
  "Queries the user for a nugget or wildcard list of
nuggets. Behaves differently, depending on whether
geex-sqlalchemy-filter-by-tag-parents returns a single-item
list (just open that nugget) or a multi-item list because the
query included wildcards (in which case, embed all the matching
nuggets in a temp buffer).

This is the main function for opening (and even creating) geex
nuggets."

  (interactive)

  ;; get-query takes in the query as input through the
  ;; minibuffer, which we then feed to filter-by-tag-parents
  ;; to return the nugget(s) that match the query
  (let* ((query (geex-meta-get-query))
         (chosen
           (geex-sqlalchemy-filter-by-tag-parents query nil "like"))
         (filename nil))

    (cond

      ;; the list matching the query is empty
     ((equal (length chosen) 0)
      ;; create a new nugget
      (setq filename (concat (file-name-as-directory geex-mode-dir)
                             query "." geex-mode-ext))
      (geex-log "In geex-meta-find, new filename: %s" filename)
      (find-file filename)
      ;; add the nugget name at the top of the new nugget
      (cond ((eq geex-mode-selection 'geex)
             (geex-meta-new-file-header query))
            ((eq geex-mode-selection 'muse)
             (geex-meta-new-muse-file-header query)))
      (clear-visited-file-modtime))

      ;; try and open this single nugget
     ((equal (length chosen) 1)
      (setq chosen (car chosen))
      ;; check that the user hasn't given us an empty alias
      (unless (> (length chosen) 1)
        (error "Can't open an empty alias"))
      (setq filename (geex-sqlalchemy-get-filename-a
                      (geex-meta-remove-parents-from-alias-chosen chosen)))
        ;; if the filename doesn't exist in the db, then we're
        ;; going to have to create a new file called
        ;; CHOSEN.geex
      (when (not filename)
        (setq filename (geex-sqlalchemy-add-ext chosen)))
      ;; get the full file name path by prepending the
      ;; geex-mode-dir and appending the extension
      (setq filename (concat (file-name-as-directory geex-mode-dir)
                             filename))
      ;; ok. now we can either load or create our new file
        (find-file filename))

     ;; create a temp buffer into which to embed the
     ;; multiple nuggets in this list
     ((> (length chosen) 1)
      (geex-meta-insert-embedded-tag-children-in-temp-buffer query)))))

(defun geex-meta-complete-alias-filename (&optional prompt)
  "Asks the user for an alias, and then returns its filename."
  (geex-sqlalchemy-get-filename-a
   (geex-meta-complete-alias prompt t)))

(defun geex-meta-complete-alias-new (&optional prompt require-match in-in)
  (unless prompt
    (setq prompt "New nugget: "))

  (let ((chosen
         (geex-meta-complete-alias prompt require-match (concat in-in " - "))))

    (when (geex-sqlalchemy-exist-nugget-a chosen)
      (error "A nugget of that name already exists"))

    chosen))

;; (insert (format "%S"
;;     (dynamic-completion-table geex-sqlalchemy-filter-by-tag-parents)))
(defun geex-meta-filter-by-tag-parents-dct (string predicate mode)

  ;; trying to fix the issue where SPC doesn't add spaces
  ;; for unknown aliases, but this didn't have any effect
  ;;
  ;; (local-set-key "SPC" (lambda () (insert " ")))
  (with-current-buffer
      (let ((window (minibuffer-selected-window)))
        (if (window-live-p window)
            (window-buffer window)
          (current-buffer)))
    (cond
     ((eq mode t)
      ;; gives the list of completions. this seems to be
      ;; working fine
      (all-completions
       string
       (geex-sqlalchemy-filter-by-tag-parents string)
       predicate)
      )
     ((not mode)
      ;; returns common substring of all completions of
      ;; STRING in ALIST, i.e. completes as far as it can
      ;; before things get ambiguous
      (try-completion
       string
       (geex-sqlalchemy-filter-by-tag-parents string)
       predicate)
      )
     (t
      ;; returns non-nil if STRING is a valid completion
      (test-completion
       string
       (geex-sqlalchemy-filter-by-tag-parents string)
       predicate)
      ))))

;; tell what we provide
(provide 'geex-meta)
