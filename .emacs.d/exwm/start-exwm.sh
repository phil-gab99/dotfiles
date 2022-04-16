#!/bin/bash
xset dpms 0 0 0 && xset -dpms && xset s off && xset s noblank # Prevent screen blank

xinput set-prop "PNP0C50:00 04F3:311D Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "ETPS/2 Elantech Touchpad" "libinput Tapping Enabled" 1
xinput set-prop "PNP0C50:00 04F3:311D Touchpad" "libinput Natural Scrolling Enabled" 1
xinput set-prop "ETPS/2 Elantech Touchpad" "libinput Natural Scrolling Enabled" 1

xinput map-to-output "Wacom Intuos PT S Pen stylus" eDP-1     # Maps stylus to laptop monitor
xinput map-to-output "Wacom Intuos PT S Pen eraser" eDP-1     # Maps eraser to laptop monitor

start_daemons () {
    eval "$(gnome-keyring-daemon --start --components=ssh)"
    compton &
    gnome-keyring-daemon --start --components=pkcs11 &
    gnome-keyring-daemon --start --components=secrets &
    /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
    export SSH_AUTH_SOCK
}

exwm () {
    export EXWM=1
    # Disable access control for the current user.
    # xhost "+SI:localuser:$USER"

    # Make Java applications aware this is a non-reparenting window manager.
    export _JAVA_AWT_WM_NONREPARENTING=1

    # Set default cursor.
    xsetroot -cursor_name left_ptr

    # Set keyboard repeat rate.
    # xset r rate 200 60

    # Finally start Emacs
    # exec dbus-launch emacs --eval "(lerax-exwm-start nil t)"
    exec dbus-launch --exit-with-session emacs -mm -debug-init -l ~/.emacs.d/desktop.el
}

start_daemons
exwm
