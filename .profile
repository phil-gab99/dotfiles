################################################################################
# Loading profiles/environments
################################################################################

# Load the default Guix profile
GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE"/etc/profile

# Load additional Guix profiles
export GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
for i in $GUIX_EXTRA_PROFILES/*; do
    profile=$i/$(basename "$i")
    if [ -f "$profile"/etc/profile ]; then
        GUIX_PROFILE="$profile"
        . "$GUIX_PROFILE"/etc/profile
    fi
    unset profile
done

unset i

# Load Nix environment
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
    . /run/current-system/profile/etc/profile.d/nix.sh
fi

################################################################################
# Path variables
################################################################################

paths=(
    # Make user scripts and other executables visible to Path
    "$HOME/bin"
    "$HOME/Packages"

    # Make npm apps visible to Path
    "$HOME/.npm/bin"

    # Make Nix apps visible to Path
    "$HOME/.nix-profile/bin"
)
for p in ${paths[@]}; do
    PATH="$PATH:$p"
done
export PATH

# Append libraries from Nix user packages to library path
export LIBRARY_PATH="$LIBRARY_PATH:$HOME/.nix-profile/lib"

# Append LD libraries for shared objects libraries
export LD_LIBRARY_PATH="$GUIX_EXTRA_PROFILES/build-tools/build-tools/lib"

paths=(
    # Make Flatpak apps visible to launcher
    "$HOME/.local/share/flatpak/exports/share"

    # Make Nix apps visible to launcher and Path
    "$HOME/.nix-profile/share"

    # Make applications in other profiles visible to launcher
    "$GUIX_EXTRA_PROFILES/audio/audio/share"
    "$GUIX_EXTRA_PROFILES/browsers/browsers/share"
    "$GUIX_EXTRA_PROFILES/games/games/share"
    "$GUIX_EXTRA_PROFILES/latex/latex/share"
    "$GUIX_EXTRA_PROFILES/media/media/share"
    "$GUIX_EXTRA_PROFILES/themes-fonts/themes-fonts/share"
)
for p in ${paths[@]}; do
    XDG_DATA_DIRS="$XDG_DATA_DIRS:$p"
done
export XDG_DATA_DIRS

# Some scripts make use of path to config directory
export XDG_CONFIG_HOME="$HOME/.config"

# Some script make use of path to cache directory
export XDG_CACHE_HOME="$HOME/.cache"

paths=(
    # Make manual pages in other profiles visible   
    "$GUIX_EXTRA_PROFILES/audio/audio/share/man"
    "$GUIX_EXTRA_PROFILES/browsers/browsers/share/man"
    "$GUIX_EXTRA_PROFILES/build-tools/build-tools/share/man"
    "$GUIX_EXTRA_PROFILES/cc/cc/share/man"
    "$GUIX_EXTRA_PROFILES/desktop/desktop/share/man"
    "$GUIX_EXTRA_PROFILES/emacs/emacs/share/man"
    "$GUIX_EXTRA_PROFILES/games/games/share/man"
    "$GUIX_EXTRA_PROFILES/java/java/share/man"
    "$GUIX_EXTRA_PROFILES/latex/latex/share/man"
    "$GUIX_EXTRA_PROFILES/media/media/share/man"
    "$GUIX_EXTRA_PROFILES/perl/perl/share/man"
    "$GUIX_EXTRA_PROFILES/prolog/prolog/share/man"
    "$GUIX_EXTRA_PROFILES/python/python/share/man"
    "$GUIX_EXTRA_PROFILES/themes-fonts/themes-fonts/share/man"
    "$GUIX_EXTRA_PROFILES/video/video/share/man"
)
for p in ${paths[@]}; do
    MANPATH="$MANPATH:$p"
done
export MANPATH

paths=(
    # Make info nodes in other profiles visible
    "$GUIX_EXTRA_PROFILES/audio/audio/share/info"
    "$GUIX_EXTRA_PROFILES/browsers/browsers/share/info"
    "$GUIX_EXTRA_PROFILES/build-tools/build-tools/share/info"
    "$GUIX_EXTRA_PROFILES/cc/cc/share/info"
    "$GUIX_EXTRA_PROFILES/desktop/desktop/share/info"
    "$GUIX_EXTRA_PROFILES/emacs/emacs/share/info"
    "$GUIX_EXTRA_PROFILES/java/java/share/info"
    "$GUIX_EXTRA_PROFILES/latex/latex/share/info"
    "$GUIX_EXTRA_PROFILES/media/media/share/info"
    "$GUIX_EXTRA_PROFILES/python/python/share/info"
    "$GUIX_EXTRA_PROFILES/themes-fonts/themes-fonts/share/info"
    "$GUIX_EXTRA_PROFILES/video/video/share/info"                 
)
for p in ${paths[@]}; do
    INFOPATH="$INFOPATH:$p"
done
export MANPATH

unset p paths

################################################################################
# Configuration variables
################################################################################

# Don't use the system-wide PulseAudio configuration
unset PULSE_CONFIG PULSE_CLIENTCONFIG

# Export java path so that tools pick it up correctly
export JAVA_HOME=$(realpath $(dirname $(dirname $(which java))))

# Arduino home directory
export ARDUINO_HOME="$HOME/.local/share/flatpak/app/cc.arduino.arduinoide/current/active/files/Arduino"

# Export casting program for qutebrowser
export QUTE_CAST_YTDL_PROGRAM="youtube-dl"

# Make sure we can reach the GPG agent for SSH auth
export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"

# Make sure `ls` collates dotfiles first (for dired)
export LC_COLLATE="C"

# Many build scripts expect CC to contain the compiler command
export CC="gcc"

# Jupyter config file
export JUPYTER_CONFIG_DIR=$XDG_CONFIG_HOME/jupyter

# We're in Emacs
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Less specific variables
export LESSHISTFILE=$XDG_CACHE_HOME/.lesshst

# Guile specific variables
export GUILE_HISTORY=$XDG_CACHE_HOME/.guile_history

# Python specific variables
export PYTHONSTARTUP=$XDG_CONFIG_HOME/python/history.py
# [[ -v PYTHONPATH ]] \
    #     && export PYTHONPATH="$PYTHONPATH:$HOME/.nix-profile/lib/python3.9/site-packages" \
    #         || export PYTHONPATH="$HOME/.nix-profile/lib/python3.9/site-packages"

# Prolog specific variables
export LOCALSZ=32768

# Bash specific variables
export HISTFILE=$XDG_CACHE_HOME/.bash_history

# Email address
export EMAIL=pgabriel999@hotmail.com

################################################################################
# Shepherd
################################################################################

# Start the shepherd daemon
if [[ ! -S ${XDG_RUNTIME_DIR-$HOME/.cache}/shepherd/socket ]]; then
    shepherd # -l $XDG_CONFIG_HOME/shepherd/shepherd.log
fi

################################################################################
# Loading scripts
################################################################################

# System 76 charge thresholds set to safe charge
[ -f ~/bin/safe-charge ] && ~/bin/safe-charge

# Load .bashrc to get login environment
[ -f ~/.bashrc ] && . ~/.bashrc
