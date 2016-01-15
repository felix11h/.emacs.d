These emacs ".el" files were supplied by
Karoly Antal antalk@chello.hu

These .el files provide syntax highlighting for editing hoc and mod
(NEURON) files with the emacs (or variant thereof) editor.  See
http://www.neuron.yale.edu to download NEURON or find documentation.

Installation:

1. They can be installed is by using the load command at the
end of your ~/.emacs; include these two lines there:

(load "~/.emacs.d/site-lisp/neuron-hoc-mode.el") ;; includes filename
(load "~/.emacs.d/site-lisp/neuron-mod-mode.el") ;; includes filename

and of course move the site-lisp directory created when this archive
(zip file) was expanded to to the ~/.emacs.d directory.

2. Alternatively the .el files could be installed by concatenating them to
your ~/.emacs file.
