(require 'muse-mode)	     ; load authoring mode
(require 'muse-html) ; load publishing styles I use
;; (load "load_latex.el")
;; (require 'muse-latex)
;; (require 'muse-texinfo)
;; (require 'muse-docbook)
(require 'muse-wiki)
(require 'muse-colors)
;; (require 'muse-convert) ; renamed to muse-convert-latex
;; (require 'muse-journal)
;; ;; my pages
(require 'muse-project)


(setq muse-project-alist
      '(
        ("testmuse"
         ("~/dotfiles/emacs/geex/testmuse" :default "index" :author "Joe")
         (:base "html" :path "~/dotfiles/emacs/geex/testmuse"))))
