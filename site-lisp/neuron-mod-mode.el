;;; neuron-mod-mode.el --- NEURON-MOD code editing commands for Emacs
;; 
;;  Major mode for editing Neuron .mod files. Provides coloring
;;  keywords.
;;
;;--- Comments regarding the file neuron-mod-mode.el was modified from ---
;;
;; Copyright (C) 1988,94,96,2000  Free Software Foundation, Inc.

;; Maintainer: 
;; Keywords: unix, languages

;; The original of this file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;; --------------------------------------------------------------------

;;; Register this mode for files with .mod extension.

(add-to-list 'auto-mode-alist '("\\.mod" . neuron-mod-mode))

;;; Code:


(require 'cc-mode)
(c-initialize-cc-mode)

(defvar neuron-mod-mode-syntax-table nil
  "Syntax table in use in Neuron-Mod-mode buffers.")

(if nil ; neuron-mod-mode-syntax-table
    ()
  ;;else
  (setq neuron-mod-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?: "<   " neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?\r ">   " neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " neuron-mod-mode-syntax-table)

  ;;(modify-syntax-entry ?\\ "\\" neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?\n ">   " neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?\f ">   " neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?\# "<   " neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?/ "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?* "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?+ "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?- "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?= "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?% "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?< "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?> "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?& "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?| "." neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?_ "_" neuron-mod-mode-syntax-table)
  ;;(modify-syntax-entry ?\' "\"" neuron-mod-mode-syntax-table)

  ;; Populate the syntax TABLE
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?_  "_"     neuron-mod-mode-syntax-table)
  ;;  (modify-syntax-entry ?\\ "\\"    neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?+  "."     neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?-  "."     neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?=  "."     neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?%  "."     neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?<  "."     neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?>  "."     neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?&  "."     neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?|  "."     neuron-mod-mode-syntax-table)
  (modify-syntax-entry ?\' "\""    neuron-mod-mode-syntax-table)
   (modify-syntax-entry ?/ "." neuron-mod-mode-syntax-table)     ; ACM 2002/4/27.  
    (modify-syntax-entry ?* "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?+ "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?- "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?= "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?% "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?< "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?> "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?& "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?| "." neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?_ "_" neuron-mod-mode-syntax-table)
    (modify-syntax-entry ?\' "." neuron-mod-mode-syntax-table)
    ;; Set up block and line oriented comments.  The new C standard
    ;; mandates both comment styles even in C, so since all languages
    ;; now require dual comments, we make this the default.

    ;;  (cond


    ;;    ;; XEmacs
    ;;    ((memq '8-bit c-emacs-features)
    ;;     (modify-syntax-entry ?/  ". 1456" neuron-mod-mode-syntax-table)
    ;;     (modify-syntax-entry ?*  ". 23"   neuron-mod-mode-syntax-table))
    ;;    ;; Emacs
    ;;    ((memq '1-bit c-emacs-features)
    ;;     (modify-syntax-entry ?/  ". 124b" neuron-mod-mode-syntax-table)
    ;;     (modify-syntax-entry ?*  ". 23"   neuron-mod-mode-syntax-table))
    ;;    ;; incompatible
    ;;    (t (error "CC Mode is incompatible with this version of Emacs")))

    ;;   (modify-syntax-entry ?\n "> b"  neuron-mod-mode-syntax-table)
    ;;   ;; Give CR the same syntax as newline, for selective-display
    ;;   (modify-syntax-entry ?\^m "> b" neuron-mod-mode-syntax-table)

    ;;   (cond
    ;;    ;; XEmacs 19 & 20
    ;;    ((memq '8-bit c-emacs-features)
    ;;     (modify-syntax-entry ?/  ". 1456" neuron-mod-mode-syntax-table)
    ;;     (modify-syntax-entry ?*  ". 23"   neuron-mod-mode-syntax-table))
    ;;    ;; Emacs 19 & 20
    ;;    ((memq '1-bit c-emacs-features)
    
    ;;     ;;    (modify-syntax-entry ?/  ". 124b" neuron-mod-mode-syntax-table)
    ;;     (modify-syntax-entry ?/  ". 12b" neuron-mod-mode-syntax-table)
    ;;     (modify-syntax-entry ?\(  ". 1" neuron-mod-mode-syntax-table)
    ;;     (modify-syntax-entry ?\)  ". 4" neuron-mod-mode-syntax-table)
    
    ;;     (modify-syntax-entry ?*  ". 23"   neuron-mod-mode-syntax-table))
    ;;    ;; incompatible
    ;;    (t (error "CC Mode is incompatible with this version of Emacs"))
    ;;    )
    ;;   (modify-syntax-entry ?\n "> b"  neuron-mod-mode-syntax-table)
    ;;   ;; Give CR the same syntax as newline, for selective-display
    ;;   (modify-syntax-entry ?\^m "> b" neuron-mod-mode-syntax-table)

    );;if


;; Regexps written with help from Peter Galbraith <galbraith@mixing.qc.dfo.ca>.
(defconst neuron-mod-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Function names.
     '("^[ \t]*\\(PROCEDURE\\|FUNCTION\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     ;;
     '("[ \t]*\\(LOCAL\\|GLOBAL\\|RANGE\\|POINT_PROCESS\\|NONSPECIFIC_CURRENT\\|ELECTRODE_CURRENT\\|POINTER\\)\\>[ \t]*\\([_,a-zA-Z0-9 \t]+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-variable-name-face nil t))

     '("[ \t]*\\(DEFINE\\)\\>[ \t]*\\(\\sw[_a-zA-Z0-9]+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-variable-name-face nil t))

     ;; it seem DERIVATIVE intorduces a name
     '("[ \t]*\\(DERIVATIVE\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-variable-name-face nil t))

     '("[ \t]*\\(TITLE\\)\\>[ \t]*\\([\-_a-zA-Z0-9 \t]+\\)"
       (1 font-lock-keyword-face) (2 font-lock-doc-face nil nil))


     '("[ \t]+\\([_a-zA-Z0-9]+\\)\\>[ \t]*\\(=\\)"
        (1 font-lock-variable-name-face) (2 font-lock-constant-face nil t) )

     ;; Keywords.
     (regexp-opt
      '(
	"TITLE" "NEURON" "PARAMETER" "UNITS" "ASSIGNED" "STATE"
	"BREAKPOINT" "INITIAL"
	"FUNCTION" "LOCAL" "DERIVATIVE" "PROCEDURE" "SOLVE" "METHOD"
	"SUFFIX" "USEION" "READ" "WRITE" "RANGE" "POINTER" "GLOBAL"
	"KINETIC" "FROM" "TO" "COMPARTMENT" "COMMENT" "ENDCOMMENT"
	"DEFINE" "CONSTANT"
	"NONSPECIFIC_CURRENT" "STEADYSTATE" "CONSERVE"
	"TABLE" "WITH"
	"POINT_PROCESS" "NET_RECEIVE" "INDEPENDENT" "INCLUDE"
	"VERBATIM" "ENDVERBATIM"
	
	"BEFORE" "AFTER" "ELECTRODE_CURRENT"
	"UNITSOFF" "UNITSON"
	"EQUATION" "MODEL"

	

	"else" "for"

	  "new"
	"if" "elsif"  "return" "while" 

	 ;; c keywords
	 "extern"
	) 'words)
     ;;
     ;; Builtins.
     (list (regexp-opt
	    '(

	      "exp" "log" "fabs"

	      ;; get arguments in a PROCEDURE
	      "gargstr" "getarg"
	      
	      ;; predefined variables/names
	      "celsius" "v"
	      "t" "dt" ;; <-- are these predefined?
	      "ca" "cai" "cao" "eca" "ica"
	      "na" "nai" "nao" "ena" "ina"
	      "k"  "ki" "ko" "ek" "ik"
	      
	      ) 'words)
	   1 'font-lock-builtin-face)

     (list (regexp-opt
	    '(  "void" "bool" "int" "double" "string" ) 'words)
	   1 'font-lock-type-face)

     ;; words that may follow METHOD
     (cons (regexp-opt '( "cnexp" "euler" "runge" "sparse" "derivimplicit" ))
	   'font-lock-constant-face)

     ;; (some) builtin units
     (cons   "\\(milli\\|micro\\|nano\\|pico\\|\\)\\(volt\\|amp\\|ohm\\|mho\\|siemens\\|liter\\|degC\\|micron\\)" 
	   'font-lock-constant-face)

     ;; short units 
     (cons (regexp-opt '( "ms" "cm2"  ) 'words)
	   'font-lock-constant-face)
     

     ;; SUFFIX nothing in argpass.mod
     (cons (regexp-opt '( "nothing"  ))
	   'font-lock-constant-face)


     ;;
     ;; Operators.  
     (cons (regexp-opt '("&&" "="  "||" "<=" "<" ">=" ">" "==" "!="  "+" "-" "*" "/" "+=" ))
	   'font-lock-constant-face)
     ))
 "Default expressions to highlight in NEURON-MOD mode.")




;;;###autoload
(define-derived-mode neuron-mod-mode c++-mode "neuron-mod"
  "Major mode for editing neuron .mod code."

;;  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
;;  (set (make-local-variable 'paragraph-separate) paragraph-start)

  (set (make-local-variable 'comment-start) ": ")
  (set (make-local-variable 'comment-end) "" ; "[\\n\\r\\f]"
       )
  (set (make-local-variable 'comment-start-skip) ": *")

  ;;  (set (make-local-variable 'comment-start) "//")
  ;;  (set (make-local-variable 'comment-end) "\\*/")

  ;;  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|//+ *")
  ;;  (set (make-local-variable 'comment-start-regexp) "//\\|/\\*")
  ;;  (set (make-local-variable 'comment-multiline) t)

  ;;  (set (make-local-variable 'c-keywords) "real\\|int\\|bool\\|void\\|type_t\||unit_t") 

  (setq font-lock-defaults '(neuron-mod-font-lock-keywords nil nil ((?_ . "w"))))

  ;; turn off syntactic indentation 
  (set (make-local-variable 'c-syntactic-indentation) nil)

)

(provide 'neuron-mod-mode)

;;; neuron-mod-mode.el ends here
