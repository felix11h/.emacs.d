
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
;; (add-to-list 'load-path "~/.emacs.d/themes/color-theme-sanityinc-solarized")
;; (require 'color-theme-sanityinc-solarized)
;; (load-theme 'sanityinc-solarized-dark t)  ;; -light, -dark

;; solarized themes by Ethan Schoonover, adapted by Steve Purcel,
;; legacy version, adapted by me
;; https://github.com/purcell/color-theme-sanityinc-solarized
(add-to-list 'load-path "~/.emacs.d/themes/color-theme-solarized-legacy")
(require 'color-theme-sanityinc-solarized)
(load-theme 'sanityinc-solarized-dark t)  ;; -light, -dark



;; ------------ global ---------------------

(tool-bar-mode -1)                      ;;no toolbar
(menu-bar-mode -1)                      ;;no menu bar
(set-scroll-bar-mode 'left)   
(scroll-bar-mode -1)                    ;;no scrollbar

(setq inhibit-startup-screen t)
;; opening new buffers won't split the frame
(set-frame-parameter nil 'unsplittable t)  


;; ~~~~~~~~~~~~    Always ON   ~~~~~~~~~~~~~~

;;             load paths -----------

;; adds load paths of subdirs in .emacs.d/modes
(let ((base "~/.emacs.d/modes"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))



;; Fullscreen  Mode 

;; F11 = Full Screen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))3
(global-set-key [f11] 'toggle-fullscreen)

;; C-x C-f creates directory if needed
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))




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

(add-to-list 'load-path "~/.emacs.d/modes/org-mode-8.3.3/lisp")
;;(add-to-list 'load-path "~/.emacs.d/modes/org-mode-8.3.3/contrib/lisp" t)

;; enable ido in org-mode
(setq org-completion-use-ido t)

;; can be overwritten by #+STARTUP: noindent 
(setq org-startup-indented t)
;;(setq org-adapt-indentation t)

;; org-specific setting
(require 'adaptive-wrap)
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)  ;; word-wrap
            ;;(adaptive-wrap-prefix-mode 1)   ;; makes wrap look nice
            ;; (local-set-key "\C-k" 'C-u C-c C-l)
            ;;(local-set-key (kbd "C-c C-2") (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'org-insert-link))))
           ))

   


;; (package-initialize) (require 'org) (define-key org-mode-map (kbd "C-c C-k") (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively 'org-insert-link))))



;; ----------- org-link handling ----------

(setq org-return-follows-link t)
(setq org-open-non-existing-files t)

;; when doing C-c C-l and choosing file+sys: autcompletion is now available
(defun org-file+sys-complete-link (&optional arg)
  "Create a file link using completion."
  (let ((file (org-iread-file-name "File: "))
	(pwd (file-name-as-directory (expand-file-name ".")))
	(pwd1 (file-name-as-directory (abbreviate-file-name
				       (expand-file-name ".")))))
    (cond ((equal arg '(16))
	   (concat "file+sys:"
		   (abbreviate-file-name (expand-file-name file))))
	  ((string-match
	    (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
	   (concat "file+sys:" (match-string 1 file)))
	  ((string-match
	    (concat "^" (regexp-quote pwd) "\\(.+\\)")
	    (expand-file-name file))
	   (concat "file+sys:"
		   (match-string 1 (expand-file-name file))))
	  (t (concat "file+sys:" file)))))


;; ------------- org-projects ---------

(setq org-export-with-section-numbers nil) ;; no headline numbers!
(setq org-export-preserve-breaks t)

(require 'org-publish)
(setq org-publish-project-alist
      '(
        ("nb-org"
         :base-directory "~/sci/nb/"
         :base-extension "org"
         :publishing-directory "~/nb/content/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         :makeindex t
         :auto-sitemap t
         :sitemap-filename "sitemap.org"  
         :sitemap-title "Sitemap"
         )
        ("nb-static"
         :base-directory "~/sci/nb/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|mov"
         :publishing-directory "~/nb/content/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("nb" :components ("nb-org" "nb-static"))

        ("3dpp-org"
         ;; Path to your org files.
         :base-directory "~/dev/projects/3diagramspp/3diagramspp/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/dev/projects/3diagramspp/3diagramspp/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>
         )
        ("3dpp-static"
         :base-directory "~/dev/projects/3diagramspp/3diagramspp/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/dev/projects/3diagramspp/3diagramspp/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("3dpp" :components ("3dpp-org" "3dpp-static"))
        ))

(global-set-key (kbd "C-c C-1") (lambda () (interactive) (org-publish "nb")))


;; ---------- org other ------------

;;syntaxhighlighting of code blocks
(setq org-src-fontify-natively t) 

;; (setq org-image-actual-width '(400)) 

;; no flyspell for Orgmode source code blocks
(defadvice org-mode-flyspell-verify
  (after my-org-mode-flyspell-verify activate)
  "Don't spell check src blocks."
  (setq ad-return-value
        (and ad-return-value
             (not (eq (org-element-type (org-element-at-point)) 'src-block)))))









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




;; -------------- LaTeX ------------------



;; ------------ modes for syntax highlighting ---------

;;====== dockerfile mode ======= 

(add-to-list 'load-path "~/.emacs.d/modes/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;====== NEURON hoc & mod mode ======= 

(load "~/.emacs.d/site-lisp/neuron-hoc-mode.el") ;; includes filename
(load "~/.emacs.d/site-lisp/neuron-mod-mode.el") ;; includes filename
