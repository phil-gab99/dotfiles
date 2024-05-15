# Export 'SHELL' to child processes.  Programs such as 'screen' honor it and
# otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]; then
    # We are being invoked from a non-interactive shell.  If this is an SSH
    # session (as in "ssh host command"), source /etc/profile so we get PATH and
    # other essential variables.
    [[ -n "$SSH_CLIENT" ]] && . /etc/profile

    # Don't do anything else.
    return
fi

################################################################################
# SOURCED SCRIPTS
################################################################################

# Source global definitions
[ -f /etc/bashrc ] && . /etc/bashrc

# Enable bash programmable completion features in interactive shells
[ -f /run/current-system/profile/share/bash-completion/bash-completion ] && . /run/current-system/profile/share/bash-completion/bash-completion

# Anaconda script initializations
__conda_setup="$('/home/phil-gab99/.guix-extra-profiles/python/python/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    [ -f "$GUIX_EXTRA_PROFILES/python/python/etc/profile.d/conda.sh" ] && . "$GUIX_EXTRA_PROFILES/python/python/etc/profile.d/conda.sh"
fi
unset __conda_setup

# Alias definitions
[ -f ~/.bash_aliases ] && . ~/.bash_aliases

# Angular CLI autocompletion
source <(ng completion script)

################################################################################
# HISTORY CONTROL
################################################################################

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

################################################################################
# NAVIGATION
################################################################################

iatest=$(expr index "$-" i)

# Check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS
shopt -s checkwinsize

# Allow ctrl-S for history navigation (with ctrl-R)
stty -ixon

# Ignore case on auto-completion
# Show auto-completion list automatically, without double tab
# Note: bind used instead of sticking these in .inputrc
if [ $iatest -gt 0 ]; then
    bind "set show-all-if-ambiguous On";
    bind "set completion-ignore-case on";
fi

################################################################################
# COLORED COMMANDS
################################################################################

# To have colors for ls and all grep commands such as grep, egrep and zgrep
export CLICOLOR=1
export LS_COLORS='no=00:fi=00:di=00;33:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:*.xml=00;31:'

# Color for manpages in less makes manpages a little easier to read
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
export LESS_TERMCAP_ue=$'\E[0m'

################################################################################
# SPECIAL FUNCTIONS
################################################################################

# Parses git current branch when inside git project
function parse_git_branch {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

# Enable session to send information to vterm via properly escaped sequences
function vterm_printf {
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Directory tracking in vterm
function vterm_prompt_end {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}

# elisp commands in vterm
function vterm_cmd {
    local vterm_elisp=""

    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

# Completely clear buffer
if [ "$INSIDE_EMACS" = 'vterm' ]; then
    function clear {
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    }
fi


################################################################################
# ELisp Functions
################################################################################

function find-file {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}

################################################################################
# COLORED PROMPT
################################################################################

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
        PS1="\[${LIGHTGRAY}\](\[${LIGHTRED}\]ERROR\[${LIGHTGRAY}\])-(\[${MAGENTA}\]Exit Code \[${LIGHTRED}\]${LAST_COMMAND}\[${LIGHTGRAY}\])-(\[${LIGHTGRAY}\]"
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
    PS1+="\n\[${LIGHTGRAY}\]┌─[\[${CYAN}\] $(date +%a) $(date +%b-'%-d') $(date +'%-I':%M:%S%P)\[${LIGHTGRAY}\]]─"

    # CPU
    PS1+="[\[${MAGENTA}\] CPU $(cpu)%"

    # Jobs
    PS1+="\[${LIGHTGRAY}\]: \[${MAGENTA}\] \j"

    # Network Connections (for a server - comment out for non-server)
    # PS1+="\[${LIGHTGRAY}\]:\[${MAGENTA}\]Net $(awk 'END {print NR}' /proc/net/tcp)"

    PS1+="\[${LIGHTGRAY}\]]─"

    # Anaconda environment
    PS1+="\[${LIGHTGRAY}\][\[${LIGHTRED}\] $CONDA_DEFAULT_ENV\[${LIGHTGRAY}\]]─"

    # Git branch
    local BRANCH=$(parse_git_branch)
    if [ "$BRANCH" != "" ]; then
        PS1+="\[${LIGHTGRAY}\][\[${LIGHTGREEN}\] $BRANCH"

        # Comparison with upstream
        count=$(git rev-list --count --left-right @{upstream}..HEAD 2>/dev/null)
        case "$count" in
            "") p="";;      # No upstream
            "0	0") p="=";; # Equal to upstream
            "0	"*) p=">";; # Ahead of upstream
            *"	0") p="<";; # Behind upstream
            *) p="<>";;     # Diverged from upstream
        esac
        PS1+="$p"

        # Unstaged changes
        if [ "$(git ls-files -dm --exclude-standard 2>/dev/null)" ]; then
            PS1+="*"
        fi

        # Untracked files
        if [ "$(git ls-files -o --exclude-standard 2>/dev/null)" ]; then
            PS1+="%"
        fi

        # Staged changes
        if [ "$(git status -s 2>/dev/null | grep "^[MTADRCU]" 2>/dev/null)" ]; then
            PS1+="+"
        fi

        PS1+="\[${LIGHTGRAY}\]]─"
    fi

    # Total size of files in current directory
    PS1+="[\[${GREEN}\] $(ls -lah | command grep -m 1 total | sed 's/total //')\[${LIGHTGRAY}\]: "

    # Number of files
    PS1+="\[${GREEN}\] $(ls -A | wc -l)\[${LIGHTGRAY}\]]"

    PS1+="\n├─"

    # User and server
    PS1+="[\[${RED}\] \u@\H"

    # Current directory
    PS1+="\[${LIGHTGRAY}\]: \[${BROWN}\] \w\[${LIGHTGRAY}\]]"

    # Skip to the next line
    PS1+="\n└─"

    # Adjust the prompt depending on whether we're in 'guix environment'.
    [ -n "$GUIX_ENVIRONMENT" ] && PS1+="[\[${YELLOW}\]λ dev\[${LIGHTGRAY}\]]"

    if [[ $EUID -ne 0 ]]; then
        PS1+="──\[${GREEN}\]■\[${NOCOLOR}\] " # Normal user
    else
        PS1+="\[${RED}\]#\[${NOCOLOR}\] " # Root user
    fi

    # vterm
    PS1=$PS1'\[$(vterm_prompt_end)\]'

    # PS2 is used to continue a command using the \ character
    [ -n "$GUIX_ENVIRONMENT" ] && PS2+="[\[${YELLOW}\]λ dev\[${LIGHTGRAY}\]]"
    PS2="\[${LIGHTGRAY}\]>\[${NOCOLOR}\] "

    # PS3 is used to enter a number choice in a script
    [ -n "$GUIX_ENVIRONMENT" ] && PS3+="[\[${YELLOW}\]λ dev\[${LIGHTGRAY}\]]"
    PS3='Please enter a number from above list: '

    # PS4 is used for tracing a script in debug mode
    [ -n "$GUIX_ENVIRONMENT" ] && PS4+="[\[${YELLOW}\]λ dev\[${LIGHTGRAY}\]]"
    PS4='\[${LIGHTGRAY}\]+\[${NOCOLOR}\] '
}

PROMPT_COMMAND='__setprompt'
