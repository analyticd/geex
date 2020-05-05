;;; geex-timestamp.el --- coloring and highlighting used by Freex

;; Copyright (C) 2007 Per B. Sederberg, Greg Detre

;; Author: Greg Detre, Per B. Sederberg
;; Keywords: hypermedia
;; Date: 

;; This file is part of Emacs Freex.  It is not part of GNU
;; Emacs.

;; Emacs Freex is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any
;; later version.

;; Emacs Freex is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public
;; License along with Emacs Freex; see the file COPYING.  If
;; not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Contributors:


;;; Code:

(require 'geex-meta)


(defun geex-timestamp-load ()
  ;; only bother doing this if 
  (when geex-mode
    (geex-sqlalchemy-add-timestamp
     (plist-get geex-embed-ov-props 'id)
     1))) ;; = 'load' action id


(defun geex-timestamp-save ()
  (geex-sqlalchemy-add-timestamp
   (plist-get geex-embed-ov-props 'id)
   2)) ;; = 'save' action id


(defun geex-timestamp-close ()
  ;; the kill-buffer-hook is getting called at save
  ;; (probably by all the temp buffer creation in
  ;; geex-embed), so we need to check that we aren't saving
  ;; before we can count this as a real buffer-closing event
  (when (not geex-embed-saving-p)
       ; (and
       ;  geex-mode
       ;  (not geex-embed-saving-p)
       ;  )
    (geex-sqlalchemy-add-timestamp
     (plist-get geex-embed-ov-props 'id)
     3))) ;; = 'close' action id


(add-hook 'kill-buffer-hook (lambda () (geex-timestamp-close)))

(add-hook 'find-file-hook (lambda () (geex-timestamp-load)))

(provide 'geex-timestamp)


