# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

#######################################################
# SOURCED ALIAS'S AND SCRIPTS
#######################################################

# Source global definitions
if [ -f /etc/bashrc ]; then . /etc/bashrc; fi

# Enable bash programmable completion features in interactive shells
if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Alias definitions
if [ -f ~/.bash_aliases ]; then . ~/.bash_aliases; fi

#######################################################
# GUIX SETUP
#######################################################

# Adjust the prompt depending on whether we're in 'guix environment'.
if [ -n "$GUIX_ENVIRONMENT" ]
then
    PS1='\u@\h \w [env]\$ '
else
    PS1='\u@\h \w\$ '
fi

#######################################################
# HISTORY CONTROL
#######################################################

# Don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend
PROMPT_COMMAND='history -a'

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

#######################################################
# NAVIGATION
#######################################################

iatest=$(expr index "$-" i)

# Check the window size after each command and, if necessary, update the values of LINES and COLUMNS
shopt -s checkwinsize

# Allow ctrl-S for history navigation (with ctrl-R)
stty -ixon

# Ignore case on auto-completion
# Note: bind used instead of sticking these in .inputrc
if [[ $iatest > 0 ]]; then bind "set completion-ignore-case on"; fi

# Show auto-completion list automatically, without double tab
if [[ $iatest > 0 ]]; then bind "set show-all-if-ambiguous On"; fi

#######################################################
# COLORED COMMANDS
#######################################################

# To have colors for ls and all grep commands such as grep, egrep and zgrep
export CLICOLOR=1
export LS_COLORS='no=00:fi=00:di=00;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:*.xml=00;31:'

# Color for manpages in less makes manpages a little easier to read
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

#######################################################
# SPECIAL FUNCTIONS
#######################################################

# Extracts any archive(s) (if unp isn't installed)
function extract {
    for archive in $*; do
        if [ -f $archive ] ; then
            case $archive in
                *.tar.bz2) tar xvjf $archive ;;
                *.tar.gz)  tar xvzf $archive ;;
                *.bz2)     bunzip2 $archive ;;
                *.rar)     rar x $archive ;;
                *.gz)      gunzip $archive ;;
                *.tar)     tar xvf $archive ;;
                *.tbz2)    tar xvjf $archive ;;
                *.tgz)     tar xvzf $archive ;;
                *.zip)     unzip $archive ;;
                *.Z)       uncompress $archive ;;
                *.7z)      7z x $archive ;;
                *)         echo "Can't extract '$archive'" ;;
            esac
        else
            echo "'$archive' is not a valid file!"
        fi
    done
}

#######################################################
# COLORED PROMPT
#######################################################

function __setprompt {
    local LAST_COMMAND=$? # Must come first!

    # Define colors
    local LIGHTGRAY="\033[0;37m"
    local WHITE="\033[1;37m"
    local BLACK="\033[0;30m"
    local DARKGRAY="\033[1;30m"
    local RED="\033[0;31m"
    local LIGHTRED="\033[1;31m"
    local GREEN="\033[0;32m"
    local LIGHTGREEN="\033[1;32m"
    local BROWN="\033[0;33m"
    local YELLOW="\033[1;33m"
    local BLUE="\033[0;34m"
    local LIGHTBLUE="\033[1;34m"
    local MAGENTA="\033[0;35m"
    local LIGHTMAGENTA="\033[1;35m"
    local CYAN="\033[0;36m"
    local LIGHTCYAN="\033[1;36m"
    local NOCOLOR="\033[0m"

    # Show error exit code if there is one
    if [[ $LAST_COMMAND != 0 ]]; then
        PS1="\[${LIGHTGRAY}\](\[${LIGHTRED}\]ERROR\[${LIGHTGRAY}\])-(\[${MAGENTA}\]Exit Code \[${LIGHTRED}\]${LAST_COMMAND}\[${LIGHTGRAY}\])-(\[${YELLOW}\]"
        if [[ $LAST_COMMAND == 1 ]]; then
            PS1+="General error"
        elif [ $LAST_COMMAND == 2 ]; then
            PS1+="Missing keyword, command, or permission problem"
        elif [ $LAST_COMMAND == 126 ]; then
            PS1+="Permission problem or command is not an executable"
        elif [ $LAST_COMMAND == 127 ]; then
            PS1+="Command not found"
        elif [ $LAST_COMMAND == 128 ]; then
            PS1+="Invalid argument to exit"
        elif [ $LAST_COMMAND == 129 ]; then
            PS1+="Fatal error signal 1"
        elif [ $LAST_COMMAND == 130 ]; then
            PS1+="Script terminated by Control-C"
        elif [ $LAST_COMMAND == 131 ]; then
            PS1+="Fatal error signal 3"
        elif [ $LAST_COMMAND == 132 ]; then
            PS1+="Fatal error signal 4"
        elif [ $LAST_COMMAND == 133 ]; then
            PS1+="Fatal error signal 5"
        elif [ $LAST_COMMAND == 134 ]; then
            PS1+="Fatal error signal 6"
        elif [ $LAST_COMMAND == 135 ]; then
            PS1+="Fatal error signal 7"
        elif [ $LAST_COMMAND == 136 ]; then
            PS1+="Fatal error signal 8"
        elif [ $LAST_COMMAND == 137 ]; then
            PS1+="Fatal error signal 9"
        elif [ $LAST_COMMAND -gt 255 ]; then
            PS1+="Exit status out of range"
        else
            PS1+="Unknown error code"
        fi
        PS1+="\[${LIGHTGRAY}\])\[${NOCOLOR}\]\n"
    else
        PS1=""
    fi

    # Date
    PS1+="\[${LIGHTGRAY}\](\[${CYAN}\]\$(date +%a) $(date +%b-'%-m')" # Date
    PS1+="${BLUE} $(date +'%-I':%M:%S%P)\[${LIGHTGRAY}\])-"

    # CPU
    PS1+="(\[${MAGENTA}\]CPU $(cpu)%"

    # Jobs
    PS1+="\[${LIGHTGRAY}\]:\[${MAGENTA}\]\j"

    # Network Connections (for a server - comment out for non-server)
    # PS1+="\[${LIGHTGRAY}\]:\[${MAGENTA}\]Net $(awk 'END {print NR}' /proc/net/tcp)"

    PS1+="\[${LIGHTGRAY}\])-"

    # User and server
    local SSH_IP=`echo $SSH_CLIENT | awk '{ print $1 }'`
    local SSH2_IP=`echo $SSH2_CLIENT | awk '{ print $1 }'`
    if [ $SSH2_IP ] || [ $SSH_IP ] ; then
        PS1+="(\[${RED}\]\u@\h"
    else
        PS1+="(\[${RED}\]\u"
    fi

    # Current directory
    PS1+="\[${LIGHTGRAY}\]:\[${BROWN}\]\w\[${LIGHTGRAY}\])-"

    # Total size of files in current directory
    PS1+="(\[${GREEN}\]$(ls -lah | grep -m 1 total | sed 's/total //')\[${LIGHTGRAY}\]:"

    # Number of files
    PS1+="\[${GREEN}\]\$(ls -A -1 | wc -l)\[${LIGHTGRAY}\])"

    # Skip to the next line
    PS1+="\n"

    if [[ $EUID -ne 0 ]]; then
        PS1+="\[${GREEN}\]$\[${NOCOLOR}\] " # Normal user
    else
        PS1+="\[${RED}\]$\[${NOCOLOR}\] " # Root user
    fi

    # PS2 is used to continue a command using the \ character
    PS2="\[${LIGHTGRAY}\]$\[${NOCOLOR}\] "

    # PS3 is used to enter a number choice in a script
    PS3='Please enter a number from above list: '

    # PS4 is used for tracing a script in debug mode
    PS4='\[${LIGHTGRAY}\]+\[${NOCOLOR}\] '
}
PROMPT_COMMAND='__setprompt'
