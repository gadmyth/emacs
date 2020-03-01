#!/bin/sh

cd ~
ln -sf emacs/snippets

if [ ! -d ~/snippets ]; then
    mkdir ~/snippets
fi

