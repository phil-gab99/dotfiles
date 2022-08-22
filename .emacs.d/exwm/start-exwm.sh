#!/bin/sh
exec dbus-launch --exit-with-session emacs -mm -debug-init -l ~/.emacs.d/lisp/pg-desktop.el
