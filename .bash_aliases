### -*- sh -*-
################################################################################
# GENERAL ALIASES
################################################################################

# Add an "alert" alias for long running commands.
# Use like so: sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# CPU usage command
alias cpu="grep 'cpu ' /proc/stat | awk '{usage=(\$2+\$4)*100/(\$2+\$4+\$5)} END {print usage}' | awk '{printf(\"%.1f\n\", \$1)}'"

# Alias to show disk space
alias diskspace="du -S | sort -n -r | more"

################################################################################
# MODIFIED COMMANDS ALIASES
################################################################################

# Grep aliases
alias grep='grep --color=always'
alias fgrep='grep -F --color=always'
alias egrep='grep -E --color=always'

# Additional flags to regular commands
alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'
alias mkdir='mkdir -p'
alias ps='ps auxf'
alias ping='ping -c 10'
alias less='less -R'

# Directory listing aliases
alias ls='ls -AFh --color=always' # Add colors and file type extensions
alias la='ls -Alh'                # Show hidden files
alias lx='ls -lXBh'               # Sort by extension
alias lk='ls -lSrh'               # Sort by size
alias lc='ls -lcrh'               # Sort by change time
alias lu='ls -lurh'               # Sort by access time
alias lr='ls -lRh'                # Recursive ls
alias lt='ls -ltrh'               # Sort by date
alias lm='ls -alh | more'         # Pipe through 'more'
alias lw='ls -xAh'                # Wide listing format
alias ll='ls -Fls'                # Long listing format
alias labc='ls -lap'              # Alphabetical sort
alias lf="ls -l | egrep -v '^d'"  # Files only
alias ldir="ls -l | egrep '^d'"   # Directories only

# Search files in the current folder
alias find="find . | grep "

# Provenance of commands including aliases
alias which='alias | which --tty-only --read-alias --show-dot --show-tilde'

################################################################################
# SYNONYMOUS ALIASES
################################################################################

# Treat Vi as Vim
alias vi='vim'

# Some software relies on 'cc' being 'gcc'
alias cc="gcc"

################################################################################
# MULTI-VERSIONED COMMANDS ALIASES
################################################################################

# alias python="python3"

################################################################################
# APPLICATION ALIASES
################################################################################

# Qutebrowser alias for some texture issues
alias qutebrowser='qutebrowser --qt-flag disable-seccomp-filter-sandbox'

# Spyder alias since web widgets cause crash
alias spyder="spyder --no-web-widgets"

# Arduino alias as it is not exported by flatpak
alias arduino="~/.local/share/flatpak/app/cc.arduino.arduinoide/current/active/files/Arduino/arduino"
