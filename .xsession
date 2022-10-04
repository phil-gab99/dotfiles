DEFAULT_SESSION=exwm

case $1 in
    *exwm)
        # Running in EXWM
        export EXWM=1

        # Ensure that font folders are loaded correctly
        xset +fp $(dirname $(readlink -f ~/.guix-extra-profiles/themes-fonts/themes-fonts/share/fonts/truetype/fonts.dir))

        # Make Java applications aware this is a non-reparenting window manager.
        export _JAVA_AWT_WM_NONREPARENTING=1

        # For debugging
        # xterm

        exec $1
        ;;
    *)
        exec $DEFAULT_SESSION
        ;;
esac
