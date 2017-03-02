
;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/modes/org-mode-8.2.10/lisp")
(require 'org) 
(require 'ox)
(require 'cl)  

(setq org-export-with-section-numbers nil)
(setq org-export-preserve-breaks t)

(setq org-export-async-debug t)







