#!/bin/env bash

set -e

SCRIPT=$(guix system vm --persistent -L $HOME/.dotfiles vm-host.scm)

guix shell virt-viewer -- $SCRIPT\
     -m 2048\
     -smp 2\
     -nic user,model=virtio-net-pci\
     -display spice-app,gl=on\
     -spice gl=on,unix=on\
     -audiodev spice,id=snd0\
     -chardev spicevmc,name=vdagent,id=vdagent

exit 0
