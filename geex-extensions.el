;;; geex-extensions.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Per-package configurations.

;;; Some helpful extensions to the geex system that I don't know where to put
;;; yet.

;;; Code:

;;; Logging
(defcustom geex-log nil
  "Set to nil to not write to *Messages* buffer."
  :type 'boolean
  :group 'geex-logging)

(defun geex-log (msg &rest args)
  "Write MSG to the *Messages* buffer."
  (when geex-log
    (setq inhibit-message t)
    (apply #'message msg args)
    (setq inhibit-message nil)))

(defun geex-embed-file-link-here ()
  "Prompt user for file path and a link description, then insert
a geex olink element and a backlink to the current file in the linked file
if such an implicit geex link doesn't already exist."
  (interactive)
  (let* ((backlink-target buffer-file-name)
         (file (read-file-name "Choose link destination (a file): " ))
         (description (read-string "Link description: "
                                   (file-name-sans-extension
                                    (file-name-nondirectory file)))))
    (insert (format "<olink targetdoc=\"%s\">%s</olink>" file description))
    ;; Open linked file, go to point-max and insert an olink to file.
    (unwind-protect
        (save-excursion
          (let ((description (file-name-sans-extension
                              (file-name-nondirectory backlink-target))))
            ;; (message "Backlink-target description: %s" description)
            (when (not (and (file-exists-p file)
                            (with-temp-buffer
                              (insert-file-contents file)
                              (goto-char (point-min))
                              (re-search-forward description nil t))))
              (find-file file)
              (save-buffer)
              (goto-char (point-max))
              (insert (format
                       "\n\nBacklink: <olink targetdoc=\"%s\">%s</olink>"
                       backlink-target description))
              (save-buffer)
              (message "Wrote backlink.")
              (find-file backlink-target)))))))

(define-key geex-mode-map (kbd "C-c F") 'geex-embed-file-link-here)

(defun geex-insert-title ()
  (interactive)
  "Insert the page title value of the buffer filename minus the path and
suffix inside a <sect level=\"1\"> element. With prefix arg, insert a markdown title instead."
  (insert
   (format (if current-prefix-arg
               "---\ntitle: %s\n---"
             "<sect level=\"1\">%s</sect>")
           (file-name-sans-extension
            (file-name-nondirectory (buffer-file-name))))))

(define-key geex-mode-map (kbd "C-c T") 'geex-insert-title)

(defun geex-embed-directory-link-here ()
  "Prompt user for directory path and a link description, then insert
  a geex olink element."
  (interactive)
  (let* ((directory (read-directory-name
                     "Choose link destination (a directory): " ))
         (description (read-string "Link description: " directory)))
    (insert (format "<olink targetdoc=\"%s\">%s</olink>"
                    directory description))))

(define-key geex-mode-map (kbd "C-c D") 'geex-embed-directory-link-here)

(defcustom geex-non-geex-file-default-directory (expand-file-name "~")
  "The directory to start a read-file-name completing read for a
non-geex file to embed."
  :type '(directory)
  :group 'geex-embed)

(defcustom geex-non-geex-embeddable-file-types
      '("tex" "latex" "kotl" "org" "txt" "csv" "cpp" "c" "C" "hs" "fs" "mm")
  "Non-geex file types that can be transcluded. User can add
  more. Some file types don't transclude/embed properly, e.g.,
  PDF."
  :type 'list
  :group 'geex-conf)

(defun geex-embed-embeddable-file-type-p (file)
  "Return non nil if `FILE' is of `GEEX-NON-GEEX-EMBEDDABLE-FILE-TYPES.'"
  (or (member (file-name-extension file) geex-non-geex-embeddable-file-types)
      (directory-name-p file)))

(defun geex-embed-non-geex-file-here ()
  "Embeds a file at point (completes to filenames), which
should get automatically fontified."
  (interactive)
  ;; ask the user to choose a file (requires match)
  (let ((filename
         (read-file-name "Embed: "
                         geex-non-geex-file-default-directory
                         nil
                         t
                         nil
                         #'geex-embed-embeddable-file-type-p)))
    (geex-xml-insert-element-at-point "file" nil filename
                                       'geex-embed-region)))

(define-key geex-mode-map (kbd "C-c e f") 'geex-embed-non-geex-file-here)

(add-to-list 'geex-fontify-elements
            '("code" t t nil geex-fontify-element-code))

(defface geex-code-face
  '((((class color) (background light))
     (:foreground "Brown"))
    (((class color) (background dark))
     (:foreground "Cyan"))
    (t (:bold t)))
  "Face for code elements when fontified."
  :group 'geex-fontify)

(defun geex-fontify-element-code (beg end attrs)
  (let ((beg-element-start beg)
        (beg-element-finish nil)
        (end-element-start nil)
        (end-element-finish end)
        (targetdoc (cdr (assoc "lang" attrs)))
        (current-font-face 'geex-code-face)
        (code-text nil))
    ;; set the missing parts of the element locations
    (save-match-data
      (goto-char beg)
      (setq beg-element-finish (and (looking-at "<[^>]+>")
                                    (match-end 0)))
      (goto-char end)
      (setq end-element-start (and (geex-looking-back "</[^>]+>")
                                   (match-beginning 0))))
    ;; see if it matches an alias
    ;; (if (geex-sqlalchemy-exist-nugget-a targetdoc)
    ;;     ;; is an alias, so set the link-file from the alias
    ;;     (setq link-file (geex-sqlalchemy-get-filename-a targetdoc))
    ;;   ;; process as a file
    ;;   (setq link-file targetdoc))
    ;; make sure file exists, if not, set link-file to nil
    ;; XXX MUST DO THIS
    ;; (when (not link-file)
    ;;   ;; change the font face to broken link
    ;;   (setq current-font-face 'geex-link-broken-face))
    ;; set the link-text
    (setq code-text (buffer-substring-no-properties
                     beg-element-finish
                     end-element-start))
    ;; set the code-text properties
    (add-text-properties beg
                         end
                         (list 'face current-font-face
                               'geex-code t
                               'keymap geex-fontify-link-local-map
                               ;;'font-lock-multiline t
                               'link-text code-text))
    ;; hide the element part
    (add-text-properties beg-element-start
                         beg-element-finish
                         (list 'invisible t))
    (add-text-properties end-element-start
                         end-element-finish
                         (list 'invisible t))))

(eval-after-load 'evil
  (fset 'geex-convert-org-links-to-olinks
        [?: ?% ?s ?/ ?\\ ?\[ ?\\ ?\[ ?\( ?. ?* ?? ?\) ?\\ ?\] ?\\ ?\[ ?\( ?. ?* ?? ?\) ?\\ ?\] ?\\ ?\] ?/ ?\\ ?< ?o ?l ?i ?n ?k ?  ?t ?a ?r ?g ?e ?t ?d ?o ?c  ?\\ ?= ?\" ?\\ ?1 ?\" ?> ?\\ ?2 ?< ?\\ ?/ ?o ?l ?i ?n ?k ?> ?/ ?g return]))

(defun geex-meta-find-other-window-maybe (arg)
  "Split window sensibly, then execute `GEEX-META-FIND.'"
  ;; Set keybinding for geex-meta-find-other-window-maybe in my-geex-config.el
  (interactive "P")
  (when arg
    (split-window-sensibly))
  (geex-meta-find))

(defun geex-fontify-buffer-cmd (limit)
  "Re-fontify the entire Freex buffer."
  (geex-fontify-region (point-min) (point-max) t))

(provide 'geex-extensions)

;;; geex-extensions.el ends here
