#!/bin/sh

mkdir ~/.local-emacs.d/
touch ~/.local-emacs.d/emacs-local.el

cd modes/helm-2.8.8
EMACSLOADPATH="/home/fh/.emacs.d/modes/emacs-async-1.9.1:" make


