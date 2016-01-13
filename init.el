
;; the MELPA package-archives, use only stable versions
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;; -------------------    themes    ------------------

;; tomorow themes by Chris Kempson, adapted by Steve Purcell
;; https://github.com/purcell/color-theme-sanityinc-tomorrow
(add-to-list 'load-path "~/.emacs.d/themes/color-theme-sanityinc-tomorrow")
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)  ;; -night, -day


