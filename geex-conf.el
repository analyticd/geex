;; Copyright 2007, Greg Detre, Per Sederberg.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; test version of geex-conf. modify then run this with
; 'emacs -q -l geex-conf-test.el' if you want to load this automatically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Then install Pymacs:
;; 2018-11-12: Using miniconda, I did: 'pip install Pymacs' followed by
;; 'pip install sqlalchemy' and added ~/.conda/bin to my zsh path.
;; or
;; pip install -e "git+https://github.com/pinard/Pymacs.git#egg=Pymacs";
;; cd $VIRTUAL_ENV/src/pymacs
;; make
;; The last two steps here are needed because of the odd build system
;; Pymacs has. You could use easy_install on a checkout of Pymacs but
;; you would still need to enter the directory and type make as python
;; setup.py install does not seem to properly build and install the
;; Pymacs.py file.
;; You can test the installation by changing to a directory that does not
;; contain Pymacs and running,
;; python -c 'import Pymacs'
;; No errors no problems.
;; Installing the Emacs files/packages of Pymacs
;; For this part I simply cloned the Pymacs repo into .emacs.d:
;; cd ~/.emacs.d
;; git clone https://github.com/pinard/Pymacs.git
;; make
;; mv Pymacs/pymacs.el geex/
;; mv Pymacs /tmp/

;; (require 'geex)
(require 'geex-mode)

;;; Marking and indenting paragraphs
(require 'geex-hiert)

;; Load pymacs
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")

;; TODO
;; (eval-after-load "pymacs"
;;   '(add-to-list 'pymacs-load-path "~/dotfiles/emacs/geex/")) ; xxx

(defcustom geex-mode-dir "~/geex/"
  "Specify where the geex .db file and its .geex (or .muse, or .org) files
will live.  (Make sure this directory exists.)"
  :type '(directory)
  :group 'geex-conf)

(defcustom geex-mode-selection
  'geex
  "Freex mode can operate in harmony (mostly) with org-mode, or
with muse mode, or without an additional major mode, i.e., in geex mode."
  :type 'string
  :options '('org 'muse 'kotl 'geex 'markdown)
  :group 'geex-conf)

;; Set the file extension that identifies geex files
(cond ((eq geex-mode-selection 'org)
       (setq geex-mode-ext "org"))
      ((eq geex-mode-selection 'muse)
       (setq geex-mode-ext "muse"))
      ((eq geex-mode-selection 'geex)
       (setq geex-mode-ext "geex"))
      ((eq geex-mode-selection 'markdown)
       (setq geex-mode-ext "md"))
      ((eq geex-mode-selection 'kotl)
       (setq geex-mode-ext "kotl"))
      (t (setq geex-mode-ext "geex")))

(defcustom geex-enable-implicit-links t
  "If non-nil, geex will activate/fontify implicit links in
nuggets. If performance is an issue, you can set this to nil."
  :type 'boolean
  :group 'geex-conf)


(defcustom geex-embed-color-step 25
  "This is the global color step size used to determine how
much to increment the color depth based on an overlay's
priority. The default (get-color-from-priority) makes
overlays' red, green and blue values darker by a stepsize of
20 (out of 256)."
  :type 'integer
  :group 'geex-conf)


(defcustom geex-completion-ignore-case t
  "Makes find-alias etc. case-insensitive."
  :type 'boolean
  :group 'geex-conf)


;; this is the regex for finding files in the directory that
;; should be inserted into the db. the caret at the
;; beginning tells it to ignore files that have a dot in
;; front (useful for excluding emacs temp files), but it
;; also excludes any files with dots in them after the first
;; character, which annoys per. since my filenames don't
;; have dots in them, this is fine for me
(setq geex-mode-dir-filter (format "^[^.#]+\\.%s$" geex-mode-ext))



(defun muse-geex-mode ()
  "Use muse mode and geex mode together."
  (when (not geex-embed-saving-p)
    ;; must load muse first
    (muse-mode)
    ;; load geex if file is in the geex dir
    (when (equal (file-name-directory (buffer-file-name)) geex-mode-dir)
      (geex-mode))))


(defun org-geex-mode ()
  "Use org-mode and geex-mode together."
  (when (not geex-embed-saving-p)
    ;; must load org first
    (org-mode)
    ;; load geex only if file is in the geex dir
    (when (equal (file-name-directory (buffer-file-name)) geex-mode-dir)
      (geex-mode))))

(defun kotl-geex-mode ()
  "Use org-mode and geex-mode together."
  (when (not geex-embed-saving-p)
    ;; must load org first
    (kotl-mode)
    ;; load geex only if file is in the geex dir
    (when (equal (file-name-directory (buffer-file-name)) geex-mode-dir)
      (geex-mode))))

(setq geex-file-suffix-regexp (format "\\.%s\\'" geex-mode-ext))
;;; Autoload configuration automatically chosen based on customization of
;;; geex-mode-selection.
(add-to-list 'auto-mode-alist
             (cons geex-file-suffix-regexp
                   (cond ((eq geex-mode-selection 'org)
                          'org-geex-mode)
                         ((eq geex-mode-selection 'muse)
                          'muse-geex-mode)
                         ((eq geex-mode-selection 'geex)
                          'geex-mode)
                         ((eq geex-mode-selection 'kotl)
                          'geex-mode)
                         (t 'geex-mode))))


(defcustom geex-content-storage 'mirror-files-to-db
  "This determines how the content of your embeddings will
be stored.

If set to 'files (the default), then geex will write the
contents of embedded overlays out to flat files, as you'd
expect.

If set to 'mirror-files-to-db, then geex
will write the contents of embedded overlays out to files,
but also to a contents field in the database. When reading,
it will read from files. This means you get all the
convenience of flat files, but all the indexing capabilities
of the db. As long as you don't manually edit the database,
things should stay perfectly in sync.

If set to 'db, geex will write the contents of embedded
overlays to the database only, i.e. there will be no flat
files. This requires you to have defined a geex-homepage,
and you will only be able to view your nuggets as embedded
overlays (since there's no way for a database record to
exist as a buffer without being associated with a file. This
hasn't been tested, and probably isn't a good idea."
  :type 'symbol
  :options '('mirror-files-to-db 'files 'db)
  :group 'geex-conf)


;; These next lines tell the database to update itself automatically
;; if there are any new files in the geex data directory, every time
;; you save
;;
;; Both these processes are resource intensive for large databases (>
;; 1000 nuggets), so you may want to comment these out and run them
;; manually in such cases.
(add-hook 'geex-embed-save-hook 'geex-meta-update-index)
(add-hook 'geex-embed-save-hook 'geex-fontify-update-implicit-link-regexp)


;; This hook automatically tokenizes the filename by hyphens, and adds
;; all the parts as tags (very handy)
(add-hook 'geex-meta-add-nugget-hooks
          'geex-meta-parse-alias-into-tag-parents)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optional keyboard shortcuts that could interfere with
;; other emacs modes ; xxx

;; i have C-l for find-file and M-l for geex-find-file
;; (global-set-key [(meta l)] 'geex-meta-find)
;; (global-set-key [(alt l)] 'geex-meta-find)
;; (global-set-key [(hyper l)] 'geex-meta-find)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-key geex-mode-map [(control =)] 'geex-embed-all-tag-children)

;; (define-key geex-mode-map "\M-s" 'save-buffer)

;; (define-key geex-mode-map "\M-DEL" 'backward-kill-word)

;; (define-key minibuffer-local-isearch-map (quote [67108921])
;;    (lookup-key minibuffer-local-isearch-map "\C-s"))

(define-key geex-mode-map (kbd "RET") 'geex-hiert-newline-and-indent)
(define-key geex-mode-map [(shift return)] 'newline)

;;; NOTE Inteferes with org-meta-return for list items (when using extended org
;;; functionality).
;; (define-key geex-mode-map [(meta return)]
;;   'geex-meta-define-new-or-insert-metadata)

(define-key geex-mode-map [(meta shift return)]
  'geex-meta-edit-tag-parents-in-minibuffer)

(define-key geex-mode-map "\M-." 'geex-hiert-in2)
(define-key geex-mode-map "\M-," 'geex-hiert-out2)

(define-key geex-mode-map (kbd "C-M-1") 'geex-fontify-insert-sect-element-1)
(define-key geex-mode-map (kbd "C-M-2") 'geex-fontify-insert-sect-element-2)
(define-key geex-mode-map (kbd "C-M-3") 'geex-fontify-insert-sect-element-3)
(define-key geex-mode-map (kbd "C-M-4") 'geex-fontify-insert-sect-element-4)

;;; Load my extensions
(require 'geex-extensions)


(provide 'geex-conf)
