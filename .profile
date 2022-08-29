# Append user scripts to path
export PATH="$PATH:$HOME/bin"

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

# Load Nix environment
if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then
  . /run/current-system/profile/etc/profile.d/nix.sh
fi

# Append libraries from Nix user packages to library path
export LIBRARY_PATH="$LIBRARY_PATH:$HOME/.nix-profile/lib"

# Don't use the system-wide PulseAudio configuration
#unset PULSE_CONFIG
#unset PULSE_CLIENTCONFIG

# Export ssh-agent socket
export SSH_AUTH_SOCK=${XDG_RUNTIME_DIR-$HOME/.cache}/ssh-agent/socket

# Export java path so that tools pick it up correctly
export JAVA_HOME=$(realpath $(dirname $(dirname $(which java))))

# Export casting program for qutebrowser
export QUTE_CAST_YTDL_PROGRAM="youtube-dl"

# Make sure we can reach the GPG agent for SSH auth
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# Make sure `ls` collates dotfiles first (for dired)
export LC_COLLATE="C"

# Many build scripts expect CC to contain the compiler command
export CC="gcc"

# Some scripts make use of path to config directory
export XDG_CONFIG_HOME="$HOME/.config"

# Some script make use of path to cache directory
export XDG_CACHE_HOME="$HOME/.cache"

# Make Flatpak apps visible to launcher
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"

# Make applications in other profiles visible to launcher
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$GUIX_EXTRA_PROFILES/audio/audio/share"
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$GUIX_EXTRA_PROFILES/browsers/browsers/share"
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$GUIX_EXTRA_PROFILES/latex/latex/share"
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$GUIX_EXTRA_PROFILES/media/media/share"
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$GUIX_EXTRA_PROFILES/themes-fonts/themes-fonts/share"

# Make manual pages in other profiles visible
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/audio/audio/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/browsers/browsers/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/build-tools/build-tools/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/cc/cc/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/desktop/desktop/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/emacs/emacs/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/java/java/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/latex/latex/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/media/media/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/python/python/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/themes-fonts/themes-fonts/share/man"
export MANPATH="$MANPATH:$GUIX_EXTRA_PROFILES/video/video/share/man"

# Make info nodes in other profiles visible
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/audio/audio/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/browsers/browsers/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/build-tools/build-tools/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/cc/cc/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/desktop/desktop/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/emacs/emacs/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/java/java/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/latex/latex/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/media/media/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/python/python/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/themes-fonts/themes-fonts/share/info"
export INFOPATH="$INFOPATH:$GUIX_EXTRA_PROFILES/video/video/share/info"

# Ensure that font folders are loaded correctly
xset +fp $(dirname $(readlink -f ~/.guix-extra-profiles/themes-fonts/themes-fonts/share/fonts/truetype/fonts.dir))

# We're in Emacs
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Less specific variables
export LESSHISTFILE=$XDG_CACHE_HOME/.lesshst

# Python specific variables
export PYTHONHISTORY=$XDG_CACHE_HOME/.python_history
export PYTHONPATH="$PYTHONPATH:$HOME/.nix-profile/lib/python3.9/site-packages"

# Bash specific variables
export HISTFILE=$XDG_CACHE_HOME/.bash_history

Start the shepherd daemon
if [[ ! -S ${XDG_RUNTIME_DIR-$HOME/.cache}/shepherd/socket ]]; then
    shepherd -l $XDG_CONFIG_HOME/shepherd/shepherd.log
fi

# Load .bashrc to get login environment
[ -f ~/.bashrc ] && . ~/.bashrc

xset dpms -2 0 0 && xset -dpms && xset s off && xset s noblank # Prevent screen blank

xinput set-prop "PNP0C50:00 04F3:311D Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "ETPS/2 Elantech Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "PNP0C50:00 04F3:311D Touchpad" "libinput Natural Scrolling Enabled" 1
xinput set-prop "ETPS/2 Elantech Touchpad" "libinput Natural Scrolling Enabled" 1

exwm () {
    export EXWM=1

    # Make Java applications aware this is a non-reparenting window manager.
    export _JAVA_AWT_WM_NONREPARENTING=1

    # Set default cursor.
    xsetroot -cursor_name left_ptr

    # For debugging
    # xterm
}

exwm
