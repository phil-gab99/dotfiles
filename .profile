# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export QUARTUS_ROOTDIR="/home/phil-gab99/Packages/Quartus/altera/13.0sp1/quartus"
# export PRINTER="Canon_MF642C_643C_644C"
export PRINTER="Canon_MF642C_643C_644C_aa_7c_a2_8_"
# export QUARTUS_ROOTDIR_OVERRIDE="/home/phil-gab99/Packages/Quartus/altera/13.0sp1/quartus"
PATH="$PATH:$QUARTUS_ROOTDIR/bin:$QUARTUS_ROOTDIR_OVERRIDE/bin"
