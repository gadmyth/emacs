#!/bin/sh

cd ~
ln -sf emacs/init-script/.emacs
ln -sf emacs/init-script/.emacs.frame.el
ln -sf emacs/init-script/.emacs.shell.el
ln -sf emacs/init-script/emacs.init.el
ln -sf emacs/el-pre-scripts
ln -sf emacs/el-extends
ln -sf emacs/snippets

if [ ! -d ~/snippets ]; then
    mkdir ~/snippets
fi

