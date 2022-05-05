#export QUARTUS_ROOTDIR="/home/phil-gab99/Packages/Quartus/altera/13.0sp1/quartus"
#export PRINTER="Canon_MF642C_643C_644C_aa_7c_a2_8_"
#export PATH="$PATH:$QUARTUS_ROOTDIR/bin:$QUARTUS_ROOTDIR_OVERRIDE/bin"
export PATH="$HOME/bin:$PATH"

# Load the default Guix profile
GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE"/etc/profile

# Load additional Guix profiles
GUIX_EXTRA_PROFILES=$HOME/.guix-extra-profiles
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

# Don't use the system-wide PulseAudio configuration
#unset PULSE_CONFIG
#unset PULSE_CLIENTCONFIG

# Export java path so that tools pick it up correctly
export JAVA_HOME=$(dirname $(dirname $(realpath $(which java))))

# Make sure we can reach the GPG agent for SSH auth
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# Make sure `ls` collates dotfiles first (for dired)
export LC_COLLATE="C"

# Many build scripts expect CC to contain the compiler command
export CC="gcc"

# Make Flatpak apps visible to launcher
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"

# Make applications in other profiles visible to launcher
#export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.guix-extra-profiles/music/music/share"
#export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.guix-extra-profiles/video/video/share"
#export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.guix-extra-profiles/browsers/browsers/share"

# Ensure that font folders are loaded correctly
xset +fp $(dirname $(readlink -f ~/.guix-extra-profiles/desktop/desktop/share/fonts/truetype/fonts.dir))

# We're in Emacs
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Load .bashrc to get login environment
[ -f ~/.bashrc ] && . ~/.bashrc
