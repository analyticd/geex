;; Copyright 2007, Greg Detre, Per Sederberg.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; test version of geex-conf. modify then run this with 
; 'emacs -q -l geex-conf-test.el' if you want to load this automatically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'geex)
(require 'geex-hiert)

;; Add geex scripts (lisp and python files) to the load path ; xxx
(add-to-list 'load-path "/Users/greg/elisp/geex/")

;; Load pymacs
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "/Users/greg/elisp/geex/")) ;; xxx

;; Specify where the geex .db file and its .geex (or .muse) friends
;; will live.  (Make sure this directory exists.)
(setq geex-mode-dir "/Users/greg/elisp/geex/demodocs/") ; xxx

;; Set the file extension that identifies geex files
(setq geex-mode-ext "geex")
;; Muse(XXX): Change "geex" to "muse" if you are using Muse with Freex
;; (see below)

;; this is the regex for finding files in the directory that
;; should be inserted into the db. the caret at the
;; beginning tells it to ignore files that have a dot in
;; front (useful for excluding emacs temp files), but it
;; also excludes any files with dots in them after the first
;; character, which annoys per. since my filenames don't
;; have dots in them, this is fine for me
(setq geex-mode-dir-filter "^[^.#]+\\.geex$")
;; Muse (XXX): Change geex to muse if you are using Muse and Freex together.


;; Set it to load geex-mode when it sees a .geex
;; file
(setq auto-mode-alist
      (cons '("\\.geex\\'" . geex-mode)
            auto-mode-alist))

;;; ;; Muse (XXX): To use Freex with Muse, comment out the above
;;; ;; auto-mode-alist entry and uncomment the following
;;; (defun muse-geex-mode ()
;;;   (when (not geex-embed-saving-p)
;;;     ;; must load muse first
;;;     (muse-mode)
;;;     ;; load geex
;;;     ;; edit so that will only go into geex if file is in the geex dir
;;;     ;;(geex-mode)))
;;;     (when (equal (file-name-directory (buffer-file-name)) geex-mode-dir)
;;;       (geex-mode))))
;;;
;;; (setq auto-mode-alist
;;;       (cons '("\\.muse\\'" . muse-geex-mode)
;;;             auto-mode-alist))


;; Specify the way to store data (we recommend that you mirror your
;; files to the database so you can make use of the full-text search
;; capabilities).
(setq geex-content-storage "mirror-files-to-db")

;; Get Freex mode ready to go
(load "/Users/greg/elisp/geex/geex-mode.el") ; xxx

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
(global-set-key [(meta l)] 'geex-meta-find)
(global-set-key [(alt l)] 'geex-meta-find)
(global-set-key [(hyper l)] 'geex-meta-find)

(define-key geex-mode-map [(control =)] 'geex-embed-all-tag-children)

(define-key geex-mode-map "\M-s" 'save-buffer)

(define-key geex-mode-map "\M-DEL" 'backward-kill-word)

;; make it a little more visible than the default 20
(setq geex-embed-color-step 25)

;; makes find-alias etc. case-insensitive
(setq completion-ignore-case t)

(define-key minibuffer-local-isearch-map (quote [67108921])
   (lookup-key minibuffer-local-isearch-map "\C-s"))

(define-key geex-mode-map (kbd "RET") 'geex-hiert-newline-and-indent)
(define-key geex-mode-map [(shift return)] 'newline)

(define-key geex-mode-map [(meta return)]
  'geex-meta-define-new-or-insert-metadata)

(define-key geex-mode-map [(meta shift return)]
  'geex-meta-edit-tag-parents-in-minibuffer)

(define-key geex-mode-map "\M-." 'geex-hiert-in2)
(define-key geex-mode-map "\M-," 'geex-hiert-out2)

(define-key geex-mode-map
  (quote [201326641]) ;; [(control meta 1)]
  'geex-fontify-insert-sect-element-1)
(define-key geex-mode-map
  (quote [201326642]) ;; [(control meta 2)]
  'geex-fontify-insert-sect-element-2)
(define-key geex-mode-map
  (quote [201326643]) ;; [(control meta 3)]
  'geex-fontify-insert-sect-element-3)
