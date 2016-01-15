;;; neuron-hoc-mode.el --- NEURON-HOC code editing commands for Emacs
;; 
;; This file defines a major mode for GNU emacs for editing Neuron
;; .hoc files. 
;;
;; It provides a coloring scheme for keywords and other predefined
;; things. 
;; 
;;
;; PROBLEM: 
;;


;; ------------ comments from the original file ---------------------
;; BUG: (* (* *) *)  First *) ends comment.
;;
;; Copyright (C) 1988,94,96,2000  Free Software Foundation, Inc.

;; Maintainer: --
;; Keywords: unix, languages


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
;; ------------------------------------------------------------------


;;; Register this mode for some extensions.

(add-to-list 'auto-mode-alist '("\\.hoc" . neuron-hoc-mode))
(add-to-list 'auto-mode-alist '("\\.ses" . neuron-hoc-mode))
(add-to-list 'auto-mode-alist '("\\.nrn" . neuron-hoc-mode))

;;; Code:


(require 'cc-mode)
(c-initialize-cc-mode)

(defvar neuron-hoc-mode-syntax-table nil
  "Syntax table in use in Neuron-Hoc-mode buffers.")

(if nil ; neuron-hoc-mode-syntax-table
    ()
  ;;else
  (setq neuron-hoc-mode-syntax-table (make-syntax-table))
  ;;(modify-syntax-entry ?\\ "\\" neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?\n ">   " neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?\f ">   " neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?\# "<   " neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?/ "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?* "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?+ "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?- "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?= "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?% "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?< "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?> "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?& "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?| "." neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?_ "_" neuron-hoc-mode-syntax-table)
  ;;(modify-syntax-entry ?\' "\"" neuron-hoc-mode-syntax-table)

  ;; Populate the syntax TABLE
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?_  "_"     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\"    neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?+  "."     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?-  "."     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?=  "."     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?%  "."     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?<  "."     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?>  "."     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?&  "."     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?|  "."     neuron-hoc-mode-syntax-table)
  (modify-syntax-entry ?\' "\""    neuron-hoc-mode-syntax-table)
  ;; Set up block and line oriented comments.  The new C standard
  ;; mandates both comment styles even in C, so since all languages
  ;; now require dual comments, we make this the default.

 (cond
   ;; XEmacs
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" neuron-hoc-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   neuron-hoc-mode-syntax-table))
   ;; Emacs
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" neuron-hoc-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   neuron-hoc-mode-syntax-table))
   ;; incompatible
   (t (error "neuron-hoc-mode is incompatible with this version of Emacs")))

  (modify-syntax-entry ?\n "> b"  neuron-hoc-mode-syntax-table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" neuron-hoc-mode-syntax-table)


);;if


;; Regexps used in matching text and associate colors to them
(defconst neuron-hoc-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Function definitions (e.g. proc myfunc )
     '("^[ \t]*\\(proc\\|func\\|obfunc\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Keywords introducing variable names.
     ;; e.g.  local a,b,c,d,e
     ;;
     '("[ \t]*\\(local\\|localobj\\|objref\\|strdef\\|objectvar\\|double\\|public\\|external\\|create\\)\\>[ \t]*\\(\\sw[_,a-zA-Z0-9 \t]*\\)?"
       (1 font-lock-keyword-face) (2 font-lock-variable-name-face nil t))

     ;; begintemplate name
     ;; endtemplate   name 
     ;; To make them stand out, I use a string color here.
     '("[ \t]*\\(begintemplate\\|endtemplate\\)\\>[ \t]*\\(\\sw[_,a-zA-Z0-9 \t]*\\)?"
       (1 font-lock-warning-face) (2 font-lock-variable-name-face nil t))

     ;;     ' class xxx ' not accepted by neuron, so commented out.
     ;;     '("[ \t]*\\(class\\)\\>[ \t]*\\(\\sw[_a-zA-Z0-9 \t]*\\)?"
     ;;       (1 font-lock-keyword-face) (2 font-lock-variable-name-face nil t))

     ;; Keywords.
     (regexp-opt
      '(  

	;;Control

        "return"
        "break"
        "continue"
        "stop"
        "if"
        "else"
        "while"
        "for"
	"iterator_statement"

	;;General Declaration

        "proc"
        "func"
	"iterator"
        "double"
        "depvar"
        "eqn"
        "local"
        "localobj"
        "strdef"

	;;Miscellaneous

        "print"
        "delete"
        "read"
        "debug"
        "em"
        "parallel"
        "help"

	;;Object Oriented

        "begintemplate"
        "endtemplate"
        "objectvar" "objref" ;; (synonyms)
	"obfunc"   ;; func that returns objref
        "public"
        "external"
        "new"

	;;Neuron Specific

        "create"        "connect"        "setpointer"        "access"        "insert"
        "uninsert"	"forall"       "ifsec"	"forsec"



	) 'words)
     ;;
     ;; Builtins.
     (list (regexp-opt
	    '(
	      ;; Built-In Functions:

	"sin" "cos" "atan" "log" "log10" "exp" "sqrt" "int" "abs"
	"erf" "erfc" "system" "prmat" "solve" "wqinit" "plt" "axis"
	"plot" "plotx" "ploty" "regraph" "symbols" "printf" "xred"
	"sred" "ropen" "wopen" "xopen" "fprint" "fscan" "graph"
	"graphmode" "fmenu" "lw" "getstr" "strcmp" "setcolor"
	"startsw" "stopsw" "object_id" "allobjectvars" "allobjexts"
	"xpanel" "xbutton" "xcheckbox" "xstatebutton" "xlabel" "xmenu"
	"xvalue" "xpvalue" "xradiobutton" "xfixedvalue" "xvarlabel"
	"xslider" "boolean_dialog" "continue_dialog" "string_dialog"
	"doEvents" "doNotify" "numarg" "hoc_pointer_" "execute"
	"execute1" "load_proc" "load_func" "load_template"
	"machine_name" "saveaudit" "retrieveaudit" "coredump_on_error"
	"checkpoint" "quit" "object_push" "object_pop" "pwman_place"
	"show_errmess_always" "numprocs" "myproc" "psync" "settext"
	"secname" 	"sprint"


	;;Variables

	"float_epsilon"
	"hoc_ac_"

	;;Names introduced by nrnoc
	;;Variables

	"clamp_resist"
	"celsius"
	"dt"
	"secondorder"
	"diam_changed"
	"stoprun"
	"t"		

	;;Functions

	"node_data" "disconnect" "batch_run" "batch_save"
	"pt3dclear" "pt3dadd" "n3d" "x3d" "y3d" "z3d" "diam3d" "arc3d"
	"define_shape" "p3dconst" "spine3d" "setSpineArea"
	"getSpineArea" "area" "ri" "initnrn" "topology" "fadvance"
	"distance"
	"finitialize" "fcurrent"
	"fstim" "fstimi" "ion_style" "nernst"
	"ghk"

	"load_file" "cvode_active"

	;;Mechanisms with Range variables

	"hh"	"pas"

	;;Classes

	"SectionRef"	"SectionList"
	"VClamp"	"SVClamp"	"IClamp"	"AlphaSynapse"
	"APCount"


	;; Missing from the above list are the built-in object classes such as
	"List" "Graph" "HBox" "File" "Deck" "Random" "Vector"
	;; and some new functions such as

       "fit_praxis" "xmenu" "xbutton" ;; etc,
       ;; as well as neuron specific classes such as
       "Shape" "SectionList" ;; etc.

       ;; The help files in NEURONHOME/lib/help are kept up to date
       ;;but this file tends to lag behind the current version

	      ) 'words)
	   1 'font-lock-builtin-face)

     ;; proc and func arguments
     (list (regexp-opt
	    '(

	      "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$8" "$9" "$10"
	      "$o1" "$o2" "$o3" "$o4" "$o5" "$o6" "$o7" "$o8" "$o9" "$o10"
	      "$s1" "$s2" "$s3" "$s4" "$s5" "$s6" "$s7" "$s8" "$s9" "$s10"

	      ;; 'scalar pointer' argument
	      "$&1" "$&2" "$&3" "$&4" "$&5" "$&6" "$&7" "$&8" "$&9" "$&10"
	      
	      "$i" ;; use variable 'i' as index into args (the name 'i' is fixed)
	      "$oi" 
	      "$si" 
	      "$&i" 

	      ;;"$[&]?[so]?\\([0-9]+\\|i\\)"
	      ) 'words)
	   1
	   ; 'font-lock-variable-name-face
	   'font-lock-warning-face
	   ; 'font-lock-comment-face
	   ; 'font-lock-reference-face
	   ; 'font-lock-function-name-face
	   )



     ;; Built-In Constants:
     (cons (regexp-opt '(
			 "PI"	"E"	"GAMMA"	"DEG"	"PHI"	"FARADAY"	"R"
			 ) 'words )
	   'font-lock-constant-face)


  

     ;;
     ;; Operators.  Is this too much?
     (cons (regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!="  "+" "-" "*" "/" "+=" "/=" "=" ))
	   'font-lock-constant-face)

     )); eval-when-compile
 "Default expressions to highlight in NEURON-HOC mode.")




;;;###autoload
(define-derived-mode neuron-hoc-mode c++-mode "neuron-hoc"
  "Major mode for editing neuron .hoc code."
;;  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
;;  (set (make-local-variable 'paragraph-separate) paragraph-start)

;;  (set (make-local-variable 'comment-start) "# ")
;;  (set (make-local-variable 'comment-end) "")
;;  (set (make-local-variable 'comment-start-skip) "#+ *")

;  (set (make-local-variable 'comment-start) "//")
;  (set (make-local-variable 'comment-end) "\\*/")

  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|//+ *")
  (set (make-local-variable 'comment-start-regexp) "//\\|/\\*")
  (set (make-local-variable 'comment-multiline) t)

  ;; turn off syntactic indentation 
  (set (make-local-variable 'c-syntactic-indentation) nil)

;;  (set (make-local-variable 'c-keywords) "real\\|int\\|bool\\|void\\|type_t\||unit_t") 

  (setq font-lock-defaults '(neuron-hoc-font-lock-keywords nil nil ((?_ . "w"))))


)

(provide 'neuron-hoc-mode)

;;; neuron-hoc-mode.el ends here
