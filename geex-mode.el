;;; geex-mode.el --- minor mode

;; Copyright (C) 2007 Greg Detre, Per B. Sederberg

;; This file is part of Emacs Freex.  It is not part of GNU Emacs.

;; Freex is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation;
;; either version 2, or (at your option) any later version.

;; Freex is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public
;; License along with Freex; see the file COPYING.  If not,
;; write to the Free Software Foundation, Inc., 51 Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Contributors:

;;; Code:

(require 'geex)
(require 'geex-embed)
(require 'geex-fontify)
(require 'geex-meta)
(require 'geex-timestamp)

(require 'font-lock)

(defun turn-on-geex-mode ()
  "Turn on GEEX-MODE."
  (interactive)
  (geex-mode 1))

(defun turn-off-geex-mode ()
  "Turn off GEEX-MODE."
  (interactive)
  (geex-mode -1))

(defun geexify ()
  (let ((filename (buffer-file-name)))
    ;; we only want to go into geex mode if there is a filename and
    (if (not filename)
        ;; can't start geex unless there is a buffer file name
        (progn
    (turn-off-geex-mode)
          (message "You can not enter geex mode if the buffer has no filename."))
      ;; is in the proper geex directory
      (if (not (equal (file-name-directory filename) geex-mode-dir))
          ;; we have a filename, but it is in the wrong directory
          ;; don't go into geex mode
          (progn
      (turn-off-geex-mode)
            (message "You can not enter geex mode when not in the proper directory."))
        ;; we are either visiting a valid geex file or a scratch buffer
        (progn
          ;; if the buffer is visiting a file (i.e. not scratch),
          ;; and that file exists, then we can try and get its id
          (if (file-exists-p filename)
              (progn
                ;; need to tell it to remove overlays before, and re-embed
                ;; them after, reverting the buffer
                ;;
                ;; the final two arguments say to prepend it (rather than
                ;; append - we don't really care) and to make this a
                ;; buffer-local variable (this is important, so that our
                ;; revert modifications only affect geex-mode
                (add-hook 'before-revert-hook
                          'geex-embed-before-revert nil t)
                (add-hook 'after-revert-hook
                          'geex-embed-after-revert nil t)
                (setq geex-embed-ov-props
                      (plist-put
                       geex-embed-ov-props 'id
                       (number-to-string
                        (geex-sqlalchemy-get-nugid-from-filename
                         (file-name-nondirectory (buffer-file-name)))))))
            ;; This is the case when we have a scratch/temporary buffer that
            ;; contains embedded nuggets.  In such a case we would like to
            ;; save the nuggets, but not save the scratch buffer holding
            ;; them.  If you would like to save the scratch buffer to file,
            ;; it is necessary to save as.
            ;;
            ;; Bleurgh. This breaks if you try and create a
            ;; new nugget by editing newnugget.geex, then
            ;; trying to save.
            (progn
              (setq geex-embed-ov-props
                    (plist-put geex-embed-ov-props 'id nil))
              ;; set the save function to null so that the base file will
              ;; not be saved to disk (and is hence a throwaway buffer)
              ;;
              ;; xxx
              ;;              (setq geex-embed-ov-props
              ;;                    (plist-put geex-embed-ov-props 'save-funct
              ;;                               'geex-embed-overlay-save-null))
              ))

          (dolist (hook geex-mode-hook)
            (eval (list hook)))
          ;; start using geex custom save
          (add-hook 'write-contents-hooks 'geex-embed-save nil t)

          ;; build the regex if this is the first time geex-mode has run
          (if (boundp 'geex-mode-has-run-already)
              nil ; do nothing
              (progn
                (setq geex-mode-has-run-already t)
                (geex-fontify-update-implicit-link-regexp)))

          (cond ((eq major-mode 'muse-mode)
                 ;; add our fontification hook to the muse-colors-region-hook
                 ;; the last argument tells it to be buffer local
                 ;; NOTE I can probably move this to geex config too.
                 (add-hook 'muse-colors-buffer-hook 'geex-fontify-region nil t))
                (t
                 ;; do our own font-locking
                 (progn
                   ;; NOTE Commented out code doesn't seem to effect outcome.
                   ;; Still works/fontifies fine.
                   ;; (set (make-local-variable 'font-lock-defaults)
                   ;; `(nil t nil nil beginning-of-line
                   ;; (font-lock-fontify-region-function . geex-fontify-region)
                   ;; (font-lock-unfontify-region-function
                   ;;  . geex-unhighlight-region)))
                   (set (make-local-variable
                         'font-lock-fontify-region-function)
                        'geex-fontify-region)
                   ;; (set (make-local-variable
                   ;;       'font-lock-unfontify-region-function)
                   ;;      'geex-unhighlight-region)
                   )))
          ;; now do all the file embedding (and fontifying of embedded files)
          (geex-embed-buffer))))))

(defun degeexify ()
  ;; save all the overlays before closing them
  (geex-embed-remove-all-geex-embed-overlays t)
  ;; stop using geex custom save
  (remove-hook 'write-contents-hooks 'geex-embed-save t)
  ;; unfontify the buffer
  (when (not (eq major-mode 'muse-mode))
    (kill-local-variable 'font-lock-defaults)
    (kill-local-variable 'font-lock-fontify-region-function)
    (kill-local-variable 'font-lock-unfontify-region-function))
  (geex-unhighlight-region (point-min) (point-max)))

(define-minor-mode geex-mode
  "Freex minor mode for editing."
   :group 'geex
   :global nil
   :init-value nil
   :lighter " Geex"
   :keymap geex-mode-map
   (when (not geex-embed-saving-p)
     (if geex-mode
         (geexify)
       (degeexify))))

(defun geex-mode-list-files ()
  "Returns a list of geex files in the `GEEX-MODE-DIR' directory."
  (mapcar 'list (directory-files
                 geex-mode-dir ;; where to look
                 nil ;; full file names?
                 geex-mode-dir-filter ;; file match regex
                 nil))) ;; nosort

;; re-evals geex-meta.el, along with a few other files, to
;; give Freex a kick-start in case pymacs crashes
(defun geex-restart ()
  (interactive)
  (load "geex.el")
  (load "geex-mode.el")
  (load "geex-embed.el")
  (load "geex-fontify.el")
  (load "geex-timestamp.el")
  (load "geex-meta.el")
  (load "geex-extensions.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'geex-mode)
