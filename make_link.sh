#!/bin/sh

cd ~
ln -sf emacs/snippets

if [ ! -d ~/snippets ]; then
    mkdir ~/snippets
fi

if [ ! -f ~/.emacs ]; then
    ln -sf ~/emacs/init-scripts/.emacs
fi
