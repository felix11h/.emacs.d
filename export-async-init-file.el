
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
(add-to-list 'load-path "~/.emacs.d/modes/ox-twbs/")

(require 'ox-twbs)
(require 'org)
(require 'htmlize)

;; ---------- only needed for org-ref ------------------- 
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

(require 'org-ref)
;; --------------------------------------------------------

(setq org-src-fontify-natively t)

;; set MathJax template
(setq org-html-mathjax-template
              "
<script type=\"text/javascript\" src=\"%PATH\"></script>
<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/

    /* 
      IMPORTANT: This template is set by 

         export-async-init-file.el

       All changes must be made there!
    */

    MathJax.Hub.Config({
        jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\", \"siunitx.js\", \"mhchem.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        TeX: {extensions: [\"AMSmath.js\",\"AMSsymbols.js\",  \"siunitx.js\", \"mhchem.js\"]},
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        /* Sets the MathJax fonts to Latin-Modern. However, the Firefox 
           path import problem strikes here again, and MathJax falls 
           back to imageFont (for which TeX is the only option). 
           See also: https://github.com/mathjax/MathJax/issues/1949

           For font differences see here: 
             https://www.checkmyworking.com/misc/MathJax-play-area/

           Changes with MathJax 3.0 on fonts are also expected.

           ADDITIONALLY, MathJax with Latin-Modern will not display 
           \mathcal{ } correctly, see 
              https://github.com/mathjax/MathJax/issues/919             
           The proposed solution is to remap \mathscr{ } to \mathcal{ },
           which for Latin-Modern displays identically and correctly.
         */

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [],
             preferredFont: \"null\",
             webFont: \"Latin-Modern\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>")


;;(require 'ox)
;;(require 'cl)  

;; this switches from inline-css to generic css syntax highlighting
;; add then to your css file color definitions, for example as created
;; with M-x org-html-htmlize-generate-css.
;; See more: http://emacs.stackexchange.com/questions/31439
(setq org-html-htmlize-output-type 'css)


(setq org-export-with-section-numbers nil)
(setq org-export-preserve-breaks t)

;; numbered lists are only parsed if written as "1."
;; lists in the form "1)" are treated as text
;; see https://stackoverflow.com/questions/14472282
(setq org-plain-list-ordered-item-terminator 46)

;; enable this for debugging
(setq org-export-async-debug nil)

;; syntaxhighlighting of code blocks







