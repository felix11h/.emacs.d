
;; the MELPA package-archives, use only stable versions
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;; -------------------    themes    ------------------

;; tomorow themes by Chris Kempson, adapted by Steve Purcell
;; https://github.com/purcell/color-theme-sanityinc-tomorrow
(add-to-list 'load-path "~/.emacs.d/themes/color-theme-sanityinc-tomorrow")
;;(require 'color-theme-sanityinc-tomorrow)
;;(load-theme 'sanityinc-tomorrow-night t)  ;; -night, -day

;; solarized themes by Ethan Schoonover, adapted by Steve Purcel,
;; slightly modified by me
;; https://github.com/purcell/color-theme-sanityinc-solarized
(add-to-list 'load-path "~/.emacs.d/themes/color-theme-sanityinc-solarized")
(require 'color-theme-sanityinc-solarized)
(load-theme 'sanityinc-solarized-dark t)  ;; -light, -dark



;; ====================    Always ON   =====================

;; --------- load paths -----------

;; adds load paths of subdirs in .emacs.d/modes
(let ((base "~/.emacs.d/modes"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))


;; ------------------ ido ------------

(require 'ido)
(ido-mode 1)
(ido-everywhere 1) ;; requested by ido-ubiquitous

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; fuzzy matching - for now probably better without?
;; (require 'flx-ido)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)



;; -------------- org-mode -------------

;;UNFINISHED!!!

;; enable ido in org-mode
(setq org-completion-use-ido t)







;; ;;(add-hook 'org-mode-hook
;; ;;          (lambda ()
;; ;;            (org-indent-mode t))
;; ;;          t)

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (visual-line-mode t))
;;           t)
   
;; ;;(setq org-log-done 'time)       ;;logging when tasks are done

;; (setq org-src-fontify-natively t) ;;syntaxhighlighting of code blocks

;; (setq org-image-actual-width '(400)) ;; set image width to 300

;; ;; no flyspell for Orgmode source code blocks
;; (defadvice org-mode-flyspell-verify
;;   (after my-org-mode-flyspell-verify activate)
;;   "Don't spell check src blocks."
;;   (setq ad-return-value
;;         (and ad-return-value
;;              (not (eq (org-element-type (org-element-at-point)) 'src-block)))))

;; ;;(add-to-list 'load-path "~/.emacs.d/org-mode-customs")
;; ;;(require 'org-expiry) 
;; ;;(org-expiry-insinuate) 
;; ;;(setq org-expiry-inactive-timestamps t)

;; ;;enables RefTeX, from http://orgmode.org/worg/org-faq.html
;; (defun org-mode-reftex-setup ()
;;   (load-library "reftex")
;;   (and (buffer-file-name)
;;        (file-exists-p (buffer-file-name))
;;        (reftex-parse-all))
;;   (define-key org-mode-map (kbd "C-c [") 'reftex-citation))
;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)


;; ;; enable languages for buffer eval and more
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    (python . t) (R . t) (sh . t) (perl . t)
;;   ))

;; ;; all python code be safe
;; (defun my-org-confirm-babel-evaluate (lang body)
;;   (not (string= lang "python")))
;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)


;; -------------- magit ----------------

;; enable ido in magit
;; (setq magit-completing-read-function 'magit-ido-completing-read)
