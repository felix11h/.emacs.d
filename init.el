
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~ Installer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; the MELPA package-archives, use only stable versions
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)



;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Themes  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; tomorow themes by Chris Kempson, adapted by Steve Purcell
;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;; -----
;; (add-to-list 'load-path "~/.emacs.d/themes/color-theme-sanityinc-tomorrow")
;; (require 'color-theme-sanityinc-tomorrow)
;; (load-theme 'sanityinc-tomorrow-night t)  ;; -night, -day

;; solarized themes by Ethan Schoonover, adapted by Steve Purcel,
;; https://github.com/purcell/color-theme-sanityinc-solarized
;; -----
;; (add-to-list 'load-path "~/.emacs.d/themes/color-theme-sanityinc-solarized")
;; (require 'color-theme-sanityinc-solarized)
;; (load-theme 'sanityinc-solarized-dark t)  ;; -light, -dark

;; solarized themes by Ethan Schoonover, adapted by Steve Purcel,
;; slightly modified by me
;; https://github.com/purcell/color-theme-sanityinc-solarized
;; -----
;; (add-to-list 'load-path "~/.emacs.d/themes/color-theme-sanityinc-solarized-legacy-2015")
;; (require 'color-theme-sanityinc-solarized)
;; (load-theme 'sanityinc-solarized-dark t)  ;; -light, -dark

;; solarized themes by Ethan Schoonover, adapted by Steve Purcel,
;; legacy version, adapted by me
;; https://github.com/purcell/color-theme-sanityinc-solarized
;; -----
(add-to-list 'load-path "~/.emacs.d/themes/color-theme-solarized-legacy")
(require 'color-theme-sanityinc-solarized)
(load-theme 'sanityinc-solarized-dark t)  ;; -light, -dark

;; Seoul 256 by Chris Davison
;; https://github.com/ChrisDavison/seoul256.el
;; ------
;; (add-to-list 'load-path "~/.emacs.d/themes/seoul256")
;; (require 'seoul256-theme)
;; (load-theme 'seoul256 t)

;; desert-theme by Sergei Lebedev
;; https://github.com/emacs-jp/replace-colorthemes/blob/master/desert-theme.el
;; -----
;; (add-to-list 'load-path "~/.emacs.d/themes/desert-theme")
;; (require 'desert-theme)
;; (load-theme 'desert t)

;; spacegray-theme by bruch
;; https://github.com/bruce/emacs-spacegray-theme
;; -----
;; (add-to-list 'load-path "~/.emacs.d/themes/emacs-spacegray-theme")
;; (require 'spacegray-theme)
;; (load-theme 'spacegray t)

;; greymatters-theme by mswift42
;; https://github.com/mswift42/greymatters-theme
;; -----
;; (add-to-list 'load-path "~/.emacs.d/themes/greymatters-theme")
;; (require 'greymatters-theme)
;; (load-theme 'greymatters t)

;; zenburn-theme by bbatsov
;; https://github.com/bbatsov/zenburn-emacs
;; -----
;; (add-to-list 'load-path "~/.emacs.d/themes/zenburn-emacs")
;; (require 'zenburn-theme)
;; (load-theme 'zenburn t)

;; solarized themes by Ethan Schoonover, implemented by Greg Pfeil
;; https://github.com/sellout/emacs-color-theme-solarized
;; -----
;; (add-to-list 'load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
;; (require 'solarized-theme)
;; (load-theme 'solarized t) 


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Global ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

(setq ring-bell-function 'ignore)               ;; no alarm when scrolling
(tool-bar-mode -1)                              ;; no toolbar
(menu-bar-mode -1)                              ;; no menu bar
(set-scroll-bar-mode 'left)                     ;; 
(scroll-bar-mode -1)                            ;; no scrollbar
(setq inhibit-startup-screen t)                 ;; no startscreen
;; opening new buffers won't split the frame
;; (set-frame-parameter nil 'unsplittable t)       ;; !!
(global-set-key "\C-z" nil)                     ;; disable minimize
(global-auto-revert-mode t)                     ;; auto refresh buffers

(column-number-mode 1)                          ;; Gives column and line
(line-number-mode 1)                            ;; number in mode line
(setq line-number-display-limit-width 2000000)  ;; prevents ?? for line numbers
                                                ;; in large files
(setq initial-scratch-message nil)              ;; suppress message in *scratch*
(setq same-window-regexps '("."))               ;; never split windows

;; ---------------    load paths    ---------------

;; adds load paths of subdirs in .emacs.d/modes
(let ((base "~/.emacs.d/modes"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(add-to-list 'load-path "~/.emacs.d/modes/pdf-tools-0.80/lisp")


;; ---------------       ido        ---------------

(require 'ido)
(ido-mode 1)
(ido-everywhere 1) ;; requested by ido-ubiquitous

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; fuzzy matching - for now probably better without?
;; (require 'flx-ido)
;; (flx-ido-mode 1)
;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)


;; ---------------   visual regexp  ---------------

(require 'visual-regexp)
(require 'visual-regexp-steroids)



;; ---------------       misc       ---------------

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
(defadvice find-file (before make-directory-maybe 
			     (filename &optional wildcards) activate)
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
		   (y-or-n-p (
		     format "Directory %s does not exist. Create it?" dir)))
	  (make-directory dir t))))))


(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))


;; ~~~~~~~~~~~~~~~~~~~~~   scale text  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'default-text-scale)

;; ~~~~~~~~~~~~~~~~~~~~-~   writeroom  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'writeroom-mode)


;; ~~~~~~~~~~~~~~~~~~~~~   sudo-edit  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(require 'sudo-edit)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~ Org-mode ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  


(add-to-list 'load-path "~/.emacs.d/modes/org-mode-8.2.10/lisp")
;;(add-to-list 'load-path "~/.emacs.d/modes/org-mode-9.0.9/lisp")
;;(add-to-list 'load-path "~/.emacs.d/modes/org-mode-8.3.3/contrib/lisp" t)

(require 'org)
(require 'ox)

;; enable ido in org-mode
(setq org-completion-use-ido t)
;; don't make ../artcl expand into ~/fh/artcl
(setq org-link-file-path-type 'relative)
;; don't ask for confirmation to execute shell links
(setq org-confirm-shell-link-function nil)


;; org-specific setting
(require 'visual-fill-column)
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)  ;; word-wrap
	    (visual-fill-column-mode 1)
	    (setq visual-fill-column-width 76)
	    (wc-mode 1) ;; word-count mode
            ;; (local-set-key (kbd "C-c C-2")  (lambda () ...
           ))


;; embbed images - [[file:image.png]] - get displayed with the
;; correct width specified by #+attr_html: :width 580px
(setq org-image-actual-width '(300))

(setq org-cycle-separator-lines 1)

;; https://emacs.stackexchange.com/questions/10981
;; https://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(setq org-ellipsis " ⮷")


;; ------------ default buffer settings ------------
;; these setting can be overriden by #+STARTUP

;; override with  #+STARTUP: noindent 
(setq org-startup-indented t)

;;(setq org-adapt-indentation t)

;; other options are: nofold, fold, content
(setq org-startup-folded 'showall)



;; --------------- org-link handling ---------------

(setq org-return-follows-link t)
(setq org-open-non-existing-files t)

;; don't split the frame when following links,
;; open in new buffer instead
(setq org-link-frame-setup
  (quote (
    (vm . vm-visit-folder-other-frame)
    (vm-imap . vm-visit-imap-folder-other-frame)
    (gnus . org-gnus-no-new-news)
    (file . find-file)
    (wl . wl-other-frame)))
  )


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

;; sets the app to use for 'file:' (but not 'file+sys:'!!) links 
(add-hook 'org-mode-hook
      '(lambda ()
         (setq org-file-apps
           '((auto-mode . emacs)
             ("\\.mm\\'" . default)
             ("\\.x?html?\\'" . default)
             ("\\.pdf\\'" . "okular %s")
	     ("\\.png\\'" . "okular %s")
	     ("\\.jpg\\'" . "okular %s")
	     ("\\.gif\\'" . "okular %s")
	     ))))

;; ---------------   org-projects   ---------------

;; !! IMPORTANT: These options are also used in export-async-init-file.el !!
(setq org-export-with-section-numbers nil)
(setq org-export-preserve-breaks t)

;; async export options
(setq org-export-async-debug nil)
(setq org-export-async-init-file
      (expand-file-name "export-async-init-file.el"
			(file-name-directory user-init-file)))

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
         :preserve-breaks nil
         )
        ("nb-static"
         :base-directory "~/sci/nb/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|mov"
         :publishing-directory "~/nb/content/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("nb" :components ("nb-org" "nb-static"))

	("dev-notes"
	 :base-directory "~/dev/note/org/"
	 :base-extension "org"
	 :publishing-directory "~/dev/note/docs/"
	 :recursive t
	 :publishing-function org-twbs-publish-to-html
	 :with-sub-superscript nil
	 :html-postamble nil
         :section-numbers nil
	 )

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
	 :table-of-contents nil
         :body-only t ;; Only export section between <body> </body>
         )
        ("3dpp-static"
         :base-directory "~/dev/projects/3diagramspp/3diagramspp/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/dev/projects/3diagramspp/3diagramspp/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("3dpp" :components ("3dpp-org" "3dpp-static"))

        ("mcp_io"
         ;; Path to your org files.
         :base-directory "~/dev/projects/mcp_io/mcp_io/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/dev/projects/mcp_io/mcp_io/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 
         :html-extension "html"
	 :table-of-contents nil
         :body-only t ;; Only export section between <body> </body>
         )

	("self_journal"
         ;; Path to your org files.
         :base-directory "~/dev/projects/self_journal/self_journal/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/dev/projects/self_journal/self_journal/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 
         :html-extension "html"
	 :table-of-contents nil
         :body-only t ;; Only export section between <body> </body>
         )

	
	("aniso_netw"
         ;; Path to your org files.
         :base-directory "~/sci/rsc/aniso_netw/pub/project_documentation/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/sci/rsc/aniso_netw/pub/project_documentation/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 
         :html-extension "html"
	 :table-of-contents nil
         :body-only t ;; Only export section between <body> </body>
         )

	("nrnd_pairs"
         ;; Path to your org files.
         :base-directory "~/sci/rsc/nrnd_pairs/pub/project_documentation/org/"
         :base-extension "org"

         ;; Path to your Jekyll project.

         :publishing-directory "~/sci/rsc/nrnd_pairs/pub/project_documentation/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4 
         :html-extension "html"
	 :table-of-contents nil
         :body-only t ;; Only export section between <body> </body>
         )

	("jrn-org"
         :base-directory "~/jrn/source/"
         :base-extension "org"
         :publishing-directory "~/jrn/content/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :auto-preamble t
         )
        ("jrn-static"
         :base-directory "~/jrn/source/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|mov"
         :publishing-directory "~/jrn/content/"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("jrn" :components ("jrn-org" "jrn-static"))
        ))



(global-set-key (kbd "C-c C-1")
  (lambda () (interactive)
    (org-publish "nb-org" :ASYNC t)

    ;; python scripts below sleep for 15 secs first to let async finish
    (start-process-shell-command "mass_replace_nb" nil "python"
                                 "~/nb/opt/tools/mass_replace.py")
    (start-process-shell-command "mass_replace_nb" nil "python"
                                 "~/nb/opt/tools/line_replace.py")

    (org-publish "nb-static" :ASYNC t)))
    

(global-set-key (kbd "C-c C-2")
		(lambda () (interactive) (org-publish "3dpp")))
(global-set-key (kbd "C-c C-3")
		(lambda () (interactive) (org-publish "mcp_io" :ASYNC t)))
(global-set-key (kbd "C-c C-4")
		(lambda () (interactive) (org-publish "self_journal" :ASYNC t)))
(global-set-key (kbd "C-c C-5")
		(lambda () (interactive) (org-publish "aniso_netw")))
(global-set-key (kbd "C-c C-6")
		(lambda () (interactive) (org-publish "nrnd_pairs")))
(global-set-key (kbd "C-c C-9")
		(lambda () (interactive) (org-publish "jrn")))


;; -----------------     ox-twbs    ------------------
(require 'ox-twbs)

;; -----------------     org-ref    ------------------
(require 'org-ref)

;; ---------------      org other      ---------------

(require 'htmlize)
;; syntaxhighlighting of code blocks
(setq org-src-fontify-natively t)

;; add more languages
(add-to-list 'org-src-lang-modes (cons "Dockerfile" 'dockerfile))

;; in-line Latex highlighting
(setq org-highlight-latex-and-related '(latex))


;; no flyspell for Orgmode source code blocks
(defadvice org-mode-flyspell-verify
  (after my-org-mode-flyspell-verify activate)
  "Don't spell check src blocks."
  (setq ad-return-value
    (and ad-return-value
      (not (eq (org-element-type (org-element-at-point)) 'src-block)))))



;;enables RefTeX, from http://orgmode.org/worg/org-faq.html
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c [") 'reftex-citation))
(add-hook 'org-mode-hook 'org-mode-reftex-setup)


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



;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Magit ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

;;disable the version control
(setq vc-handled-backends nil) 

;; (add-to-list 'load-path "~/.emacs.d/modes/magit-2.1.0/lisp")
;; (require 'magit)

;; (with-eval-after-load 'info
;;   (info-initialize)
;;   (add-to-list 'Info-directory-list
;;                "~/.emacs.d/modes/magit-2.1.0/Documentation/"))

;; enable ido in magit
;; (setq magit-completing-read-function 'magit-ido-completing-read)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ LaTeX ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 


(add-hook 'latex-mode-hook 'turn-on-reftex)
(setq reftex-default-bibliography '("~/lib/main.bib"))

;; disable AUCTeX fontification (bold fonts for sections, etc.)
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)

;; Biblatex citing http://tex.stackexchange.com/questions/31966/
(eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
	     (?o . "[[file:~/lib/notes/%l_notes.org][%l]]")
             (?f . "\\footcite[][]{%l}")
             (?t . "\\textcite[]{%l}")
             (?p . "\\parencite[]{%l}")
             (?o . "\\citepr[]{%l}")
             (?n . "\\nocite{%l}")))))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Python ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; hide-show via custom function
(add-hook 'python-mode-hook 'hs-minor-mode)

(defun my-hs-toggle-all ()
  "If anything isn't hidden, run `hs-hide-all', else run `hs-show-all'."
  (interactive)
  (let ((starting-ov-count (length (overlays-in (point-min) (point-max)))))
    (hs-hide-all)
    (when (equal (length (overlays-in (point-min) (point-max))) starting-ov-count)
      (hs-show-all))))

(defun python-mode-keys ()
  "Modify keymaps used by python mode."
  (local-set-key (kbd "C-c ;") 'comment-dwim)
  (local-set-key (kbd "<C-tab>") 'my-hs-toggle-all)
  )

(add-hook 'python-mode-hook 'python-mode-keys)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Web mode ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; !! the following line sets all .html in Django(!) mode
;;(setq web-mode-engines-alist '(("django" . "\\.html\\'")) )
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun web-mode-keybindings () 
  (define-key web-mode-map (kbd "C-c ;") 'web-mode-comment-or-uncomment)
  ) 


(add-hook 'web-mode-hook
  (lambda () ""
    (setq web-mode-markup-indent-offset 2)
    (setq-default indent-tabs-mode nil)))

(add-hook 'web-mode-hook 'web-mode-keybindings)




;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Terminal ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

(require 'multi-term)
(setq multi-term-program "/bin/bash")






;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Modes  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

;; ------------------- dockerfile mode -----------------------
(add-to-list 'load-path "~/.emacs.d/modes/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; ---------------- NEURON hoc & mod mode --------------------
(load "~/.emacs.d/site-lisp/neuron-hoc-mode.el") ;; includes filename
(load "~/.emacs.d/site-lisp/neuron-mod-mode.el") ;; includes filename

;; ----------------------- json mode -------------------------
(require 'json-mode)

;; ----------------------- git modes -------------------------
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'gitattributes-mode)

;; ----------------------- .log mode -------------------------
(require 'logview)

;; ---------------------- MATLAB mode ------------------------

(load-library "matlab-load")

;; --------------------- markdown mode -----------------------

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;; ----------------------- YAML mode -------------------------
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;; ~~~~~~~~~~~~~~~~~~~~~~ Ispell / Flyspell ~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq ispell-dictionary "en_GB")
;;(setq ispell-dictionary "en_US")
(setq ispell-personal-dictionary "~/.opt/aspell_dict/.aspell.en.pws")

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(defun add-to-dict-word-at-point ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~ langtool ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'langtool)
(setq langtool-language-tool-jar "~/opt/languagetool-3.2/languagetool-commandline.jar")
(setq langtool-mother-tongue "en")
langtool-disabled-rules '("WHITESPACE_RULE")


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~  Yasnippet  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

(require 'yasnippet)

;; load yasnippet only for specific modes:
(yas-reload-all)
(add-hook 'org-mode-hook #'yas-minor-mode)

;; default latex
(add-hook 'latex-mode-hook #'yas-minor-mode)
;; AUCTeX == 'LaTeX-mode'
(add-hook 'LaTeX-mode-hook #'yas-minor-mode)

(add-hook 'gitignore-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook #'yas-minor-mode)
(add-hook 'sh-mode-hook #'yas-minor-mode)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~  Abbrev  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(setq abbrev-file-name              ;; custom file location
      "~/.opt/abbrev/abbrev_def")    

(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

(abbrev-table-put global-abbrev-table :case-fixed t)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~ Word-count ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'wc-mode)



;; ~~~~~~~~~~~~~~~~~~~~ current date yyyy-mm-dd ~~~~~~~~~~~~~~~~~~~~~~~

(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~   GPG  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(require 'epa-file)
(epa-file-enable)

(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; Do not use gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~   Tramp  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(setq tramp-default-method "ssh")


;; ~~~~~~~~~~~~~~~~~~~~~ local machine config  ~~~~~~~~~~~~~~~~~~~~~~~~

(add-to-list 'load-path "~/.local-emacs.d/")
(load-library "emacs-local")


;; ~~~~~~~~~~~~~~~~~~~~~~~~~ Keybindings  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

(require 'bind-key)
;; bind-key* creates minor modes so that these keybindings are truly 
;; global, overwriting definitions from all other modes

;; unbinds C-i from TAB
;; see also http://emacs.stackexchange.com/a/221/338
(define-key input-decode-map [?\C-i] [C-i])
(global-set-key (kbd "<C-i>") #'previous-line)

(bind-key* "C-k" 'backward-char)

(bind-key* "C-l" 'switch-to-buffer)
(bind-key* "C-;" 'find-file)
(bind-key* "C-ö" 'find-file)

(bind-key* "C-o" 'other-window)

(bind-key* "C-c C-m" 'multi-term)

(bind-key* "C-c C-r" 'vr/replace)

(bind-key* "C-c C-a" 'beginning-of-buffer)
(bind-key* "C-c C-e" 'end-of-buffer)

(bind-key* "C-c C-h" 'magit-status)

(bind-key* "C-c i" 'add-to-dict-word-at-point)

(bind-key* "M-[" 'default-text-scale-decrease)
(bind-key* "M-]" 'default-text-scale-increase)

;;(bind-key* "C-m" 'previous-buffer)
;;(bind-key* "C-i" 'next-buffer)

(bind-key* "C-c a" (lambda() (interactive)(find-file "~/admin/top.org")))
(bind-key* "C-c s" (lambda() (interactive)(find-file "~/admin/tasks.org")))
(bind-key* "C-c o" (lambda() (interactive)(find-file "~/osc/osc_main.org")))
(bind-key* "C-c t" (lambda() (interactive)(find-file "~/admin/tech/tech_main.org")))
(bind-key* "C-c n" (lambda() (interactive)(find-file "~/sci/nb/index.org")))
(bind-key* "C-c b" (lambda() (interactive)(find-file "~/sci/nb/osc/index.org")))
(bind-key* "C-c j" (lambda() (interactive)(find-file "~/sci/main/sci_main.org")))
(bind-key* "C-c d" (lambda() (interactive)(find-file "~/dev/dev_ops.org")))
(bind-key* "C-c f" (lambda() (interactive)(find-file "~/dev/note/org/index.org")))
(bind-key* "C-c u" (lambda() (interactive)(find-file "~/admin/us/us_main.org")))
(bind-key* "C-c x" (lambda() (interactive)(find-file "~/admin/id/mcp/mcp_main.org")))

(bind-key* "C-c p" 'org-time-stamp)
(bind-key* "C-c ;" 'comment-or-uncomment-region)

(bind-key* "C-c k" (lambda() (interactive)(find-file "~/admin/events/Wikimedia_Open_Science_fellow_2017/wikimedia_fellow_2017_main.org")))
(bind-key* "C-c l" (lambda() (interactive)(find-file "~/sci/rsc/syn_lt/syn_lt_main.org")))

(bind-key* "C-c 9" (lambda() (interactive)(find-file "/ssh:hoffmann@fias.uni-frankfurt.de:tramp.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t)
     ("T1" "fontenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "amssymb" t)
     ("colorlinks=true, linkcolor=blue, urlcolor=blue, citecolor=blue" "hyperref" t)
     "\\tolerance=1000"
     ("left=1.65in, right=1.65in, top=1.25in, bottom=1.5in" "geometry" t))))
 '(safe-local-variable-values (quote ((eval org-content 2)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
