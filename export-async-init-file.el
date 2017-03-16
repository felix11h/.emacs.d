
;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (package-initialize)

;; adds load paths of subdirs in .emacs.d/modes
;; (let ((base "~/.emacs.d/modes"))
;;   (add-to-list 'load-path base)
;;   (dolist (f (directory-files base))
;;     (let ((name (concat base "/" f)))
;;       (when (and (file-directory-p name) 
;;                  (not (equal f ".."))
;;                  (not (equal f ".")))
;;         (add-to-list 'load-path name)))))


;; (add-to-list 'load-path "~/.emacs.d/themes/color-theme-solarized-legacy")
;; (require 'color-theme-sanityinc-solarized)
;; (load-theme 'sanityinc-solarized-dark t)  ;; -light, -dark

(add-to-list 'load-path "~/.emacs.d/modes/org-mode-8.2.10/lisp")
;; (add-to-list 'load-path "~/.emacs.d/modes/htmlize-1.39/")
(add-to-list 'load-path "~/.emacs.d/modes/htmlize-1.6.1/")

(require 'org)
(require 'htmlize)

(setq org-src-fontify-natively t)

;;(require 'ox)
;;(require 'cl)  

;; this switches from inline-css to generic css syntax highlighting
;; add then to your css file color definitions, for example as created
;; with M-x org-html-htmlize-generate-css.
;; See more: http://emacs.stackexchange.com/questions/31439
(setq org-html-htmlize-output-type 'css)


(setq org-export-with-section-numbers nil)
(setq org-export-preserve-breaks t)

;; enable this for debugging
(setq org-export-async-debug nil)

;; syntaxhighlighting of code blocks







