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

(defcustom geex-fontify-org-bracket-links nil
  "Fontify org bracket style links in geex mode."
  :type 'boolean
  :group 'geex-fontify)

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

(defun geex-activate-org-bracket-links ()
  "Fontify org bracket links."
  (goto-char (point-min))
  (save-excursion
      (while (org-activate-bracket-links (point-max)))))

;;; Sometimes org-mode looks messy to me. The following form gives me a lot of
;;; org-mode functionality inside geex mode files without org-mode running as
;;; major mode.
(when (not (eq geex-mode-selection 'org))
  (eval-after-load 'org
    (eval-after-load 'orgalist
      '(progn
         ;; Get some org mode goodness without running org mode as major mode.
         ;; (add-hook 'geex-mode-hook 'turn-on-orgstruct) ; removed in Org 9.2, use orgalist instead
         ;; (add-hook 'geex-mode-hook (lambda () (orgalist-mode))) ; This caused some max-size runtime errors.
         ;; (add-hook 'geex-mode-hook 'turn-on-orgtbl) ; This takes TAB away
         ;; (add-hook 'geex-mode-hook 'turn-on-cdlatex) ; I don't care for cdlatex mode.
         ;; (add-hook 'geex-mode-hook 'turn-on-filladapt-mode) ; Let's try not using this and see if the indentation issues go away.
         ;; Integrate org-mode source blocks
         (define-key geex-mode-map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
         ;; This one is buggy outside of org-mode.
         ;; (define-key geex-mode-map (kbd "C-c C-t") 'org-todo)
         (define-key geex-mode-map (kbd "C-c C-o") 'org-open-at-point)
         (define-key geex-mode-map (kbd "C-c l C-o")
           'geex-convert-org-links-to-olinks)
         ;; This next one is buggy outside of org mode.
         ;; (define-key geex-mode-map (kbd "C-c '") 'org-edit-special)
         (define-key geex-mode-map (kbd "C-c C-l") 'org-insert-link)
         (define-key geex-mode-map (kbd "C-c C-s") 'org-schedule)
         (define-key geex-mode-map (kbd "C-c C-d") 'org-deadline)
         (org-defkey geex-mode-map (kbd "C-c C-x C-l")
                     'org-toggle-latex-fragment)))))

(eval-after-load 'ag
  '(progn
     (defun geex-search-string-geex-files ()
       (interactive)
       (let ((string (read-string
                      (format "Enter the string to search for in files in your geex-mode-dir (%s): "
                              geex-mode-dir))))
         (if (featurep 'ag)
             (ag string geex-mode-dir)
           (message "Install and configure package ag to use this functionality. E.g., (package-install ag)"))))

     (define-key geex-mode-map (kbd "C-c s") 'geex-search-string-geex-files)))

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

;;; TODO Should change name to geex-append-geex-fontification-to-org and then
;;; make another version of this function called
;;; geex-append-org-fontification-to-geex. The latter will first remove all
;;; org-font-lock-extra-keywords for elements that we won't use in geex mode
;;; without org-mode running as a major mode.
(defun geex-append-fontificiation ()
  "Append call to geex-fontify-buffer-cmd to end of
org-font-lock-extra-keywords. Called by org-font-lock-set-keywords-hook. Add
(add-hook 'org-font-lock-set-keywords-hook 'geex-append-fontificiation)
in your init file for geex."
  (message "geex-append-fontification")
  (add-to-list 'org-font-lock-extra-keywords 'geex-fontify-buffer-cmd))

(defcustom geex-mode-fontify-org-elements nil
  "Fontify org elements when running in geex mode, without
org-mode running as major mode."
  :type 'boolean
  :group 'geex-conf)

;;; NOTE See TODO note on geex-append-fontificiation.
;;; TODO Will have to edit my-geex-config.el too.
(defun geex-org-fontification ()
  "Provide org fontification for org elements used in geex mode.
Presupposes geex configuration includes the following form:
(add-hook 'org-font-lock-set-keywords-hook 'geex-append-fontificiation) and
(add-hook 'geex-mode-hook 'geex-org-fontification)."
  ;; (org-load-modules-maybe)
  (add-to-list 'org-font-lock-extra-keywords 'geex-fontify-buffer-cmd)
  (org-set-font-lock-defaults)
  (font-lock-fontify-buffer))

(defun geex-fontify-org-elements ()
  "Fontify select org elements. For use when not running org as major mode."
  (message "geex-fontify-org-elements")
  (let ((keywords
         (list
          ;; Headlines
          `(,(if org-fontify-whole-heading-line
                 "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
               "^\\(\\**\\)\\(\\* \\)\\(.*\\)")
            (1 (org-get-level-face 1))
            (2 (org-get-level-face 2))
            (3 (org-get-level-face 3)))
          ;; Table lines
          '("^[ \t]*\\(\\(|\\|\\+-[-+]\\).*\\S-\\)"
            (1 'org-table t))
          ;; Table internals
          '("^[ \t]*|\\(?:.*?|\\)? *\\(:?=[^|\n]*\\)" (1 'org-formula t))
          '("^[ \t]*| *\\([#*]\\) *|" (1 'org-formula t))
          '("^[ \t]*|\\( *\\([$!_^/]\\) *|.*\\)|" (1 'org-formula t))
          '("| *\\(<[lrc]?[0-9]*>\\)" (1 'org-formula t))
          ;; Drawers
          ;; '(org-fontify-drawers)
          ;; Properties
          ;; (list org-property-re
          ;;       '(1 'org-special-keyword t)
          ;;       '(3 'org-property-value t))
          ;; Links
          (when (memq 'tag lk) '(org-activate-tags (1 'org-tag prepend)))
          (when (memq 'angle lk) '(org-activate-angle-links (0 'org-link t)))
          (when (memq 'plain lk) '(org-activate-plain-links (0 'org-link)))
          (when (memq 'bracket lk) '(org-activate-bracket-links (0 'org-link)))
          (when (memq 'radio lk) '(org-activate-target-links (1 'org-link t)))
          (when (memq 'date lk) '(org-activate-dates (0 'org-date t)))
          (when (memq 'footnote lk) '(org-activate-footnote-links))
          ;; Targets.
          ;; (list org-any-target-regexp '(0 'org-target t))
          ;; Diary sexps.
          ;; '("^&?%%(.*\\|<%%([^>\n]*?>" (0 'org-sexp-date t))
          ;; Macro
          ;; '(org-fontify-macros)
          ;; '(org-hide-wide-columns (0 nil append))
          ;; TODO keyword
          (list (format org-heading-keyword-regexp-format
                        org-todo-regexp)
                '(2 (org-get-todo-face 2) t))
          ;; DONE
          (if org-fontify-done-headline
              (list (format org-heading-keyword-regexp-format
                            (concat
                             "\\(?:"
                             (mapconcat 'regexp-quote org-done-keywords "\\|")
                             "\\)"))
                    '(2 'org-headline-done t))
            nil)
          ;; Priorities
          '(org-font-lock-add-priority-faces)
          ;; Tags
          ;; '(org-font-lock-add-tag-faces)
          ;; Tags groups
          ;; (when (and org-group-tags org-tag-groups-alist)
          ;;   (list (concat org-outline-regexp-bol ".+\\(:"
          ;;                 (regexp-opt (mapcar 'car org-tag-groups-alist))
          ;;                 ":\\).*$")
          ;;         '(1 'org-tag-group prepend)))
          ;; Special keywords
          (list (concat "\\<" org-deadline-string) '(0 'org-special-keyword t))
          (list (concat "\\<" org-scheduled-string) '(0 'org-special-keyword t))
          (list (concat "\\<" org-closed-string) '(0 'org-special-keyword t))
          (list (concat "\\<" org-clock-string) '(0 'org-special-keyword t))
          ;; Emphasis
          (when em '(org-do-emphasis-faces))
          ;; Checkboxes
          ;; '("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\(\\[[- X]\\]\\)"
          ;;   1 'org-checkbox prepend)
          ;; (when (cdr (assq 'checkbox org-list-automatic-rules))
          ;;   '("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
          ;;     (0 (org-get-checkbox-statistics-face) t)))
          ;; Description list items
          '("^[ \t]*[-+*][ \t]+\\(.*?[ \t]+::\\)\\([ \t]+\\|$\\)"
            1 'org-list-dt prepend)
          ;; ARCHIVEd headings
          ;; (list (concat
          ;;        org-outline-regexp-bol
          ;;        "\\(.*:" org-archive-tag ":.*\\)")
          ;;       '(1 'org-archived prepend))
          ;; Specials
          ;; '(org-do-latex-and-related)
          ;; '(org-fontify-entities)
          ;; '(org-raise-scripts)
          ;; Code
          '(org-activate-code (1 'org-code t))
          ;; COMMENT
          ;; (list (format
          ;;        "^\\*+\\(?: +%s\\)?\\(?: +\\[#[A-Z0-9]\\]\\)? +\\(?9:%s\\)\\(?: \\|$\\)"
          ;;        org-todo-regexp
          ;;        org-comment-string)
          ;;       '(9 'org-special-keyword t))
          ;; Blocks and meta lines
          ;; '(org-fontify-meta-lines-and-blocks)
          )))
    (setq-local font-lock-defaults
                '(keywords t nil nil backward-paragraph)))
    (kill-local-variable 'font-lock-keywords)
    (font-lock-fontify-buffer))

;;; Org agenda participation when not in org mode, i.e., when in geex mode using .geex files

;; NOTE Org agenda bombs out when non-org files are added to org-agenda-files.
;;
;; Super hacky "solution": When org-agenda is run, you could first copy any
;; geex agenda files to org-directory with suffix of org and then append it to
;; org-agenda-files.
;;
;; Each internal link would need to have its path, or, we
;; could just append a link back to the originating associated geex file so
;; that you can jump there to view the content of the todo. changed relative to
;; the new location.
(defcustom geex-agenda-files '("org-samples")
  "Files that contain scheduled org headlines that you want to be appended to org-agenda-files.")

(defun add-geex-agenda-files ()
  "Copy geex agenda files to org-directory as org files so
that they can be used in org-agenda via my-org-agenda."
  (mapc #'(lambda (filename)
            (when (file-exists-p (expand-file-name (concat filename ".from-geex.org") org-directory))
              (delete-file (expand-file-name (concat filename ".from-geex.org") org-directory) t))
            (copy-file (expand-file-name (concat filename ".geex") geex-directory)
                       (expand-file-name (concat filename ".from-geex.org") org-directory)
                       t)
            ;; TODO Parse new org file and for each schedule/deadline item, add
            ;; a link back to the original. This is a crude idea, but it is an
            ;; idea to start with at least. This can be done with a nix command
            ;; line command rather than slow elisp that involves the org-element
            ;; api. For now, I can just premeditate adding a link, via a custom,
            ;; function in the geex headline back to itself.

            ;; Append new file to org-agenda-files
            (setq org-agenda-files
                  (remove (expand-file-name (concat filename ".from-geex.org") org-directory)
                          org-agenda-files))
            (push (expand-file-name (concat filename ".from-geex.org") org-directory) org-agenda-files))
        geex-agenda-files))

;;; Used by my-org-agenda
(defun geex-available-p ()
  (featurep 'geex-mode))

(defcustom pre-org-agenda-fn-condition-pairs
  '((add-geex-agenda-files . geex-available-p))
  "If cdr of an cons funcalls to true then funcall the car of
the cons.")

(defun my-org-agenda ()
  "Before calling org-agenda, conditionally execute functions in pre-org-agenda-fn-condition-pairs."
  (interactive)
  (mapc #'(lambda (pair)
            (when (funcall (cdr pair))
              (funcall (car pair))))
        pre-org-agenda-fn-condition-pairs)
  (org-agenda))

;;; A plugged function from org to use when editing a src code buffer in geex-mode
(defun geex-set-org-src--from-org-mode ()
  (message "Setting org-src--from-org-mode to true so that org-edit-src-exit will work for geex mode.")
  (setq org-src--from-org-mode t))

(advice-add 'org-src-mode-configure-edit-buffer :before #'geex-set-org-src--from-org-mode)

(provide 'geex-extensions)

;;; geex-extensions.el ends here
