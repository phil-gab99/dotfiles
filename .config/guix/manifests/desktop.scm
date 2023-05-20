(packages->manifest
 (list
   
   ;; Settings Manager
   (specification->package "xsettingsd")
   
   (specification->package "autorandr")
   (specification->package "xrandr")
   (specification->package "arandr")
   
   (specification->package "polybar")
   
   (specification->package "dunst")
   (specification->package "libnotify") ;; For notify-send
   
   (specification->package "gucharmap")
   (specification->package "brightnessctl")
   (specification->package "xdg-utils")       ;; For xdg-open, etc
   (specification->package "xdg-dbus-proxy")  ;; For Flatpak
   (specification->package "xdg-desktop-portal-gtk")
   (specification->package+output "gtk+:bin") ;; For gtk-launch
   (specification->package "polkit-gnome")
   ;; (specification->package+output "glib:bin") ;; For gio-launch-desktop
   (specification->package "shared-mime-info")
   (specification->package "htop")
   
   (specification->package "flatpak")
   
   (specification->package "net-tools")
   (specification->package "curl")
   
   (specification->package "password-store")
   (specification->package "mcron")
   
   (specification->package "system-config-printer")
   
   (specification->package "syncthing")
   (specification->package "syncthing-gtk")
   
   (specification->package "openssh")
   (specification->package "gnupg")
   (specification->package "zip")
   (specification->package "unzip")
   (specification->package "p7zip")
   (specification->package "unrar")
   (specification->package "udiskie")
   (specification->package "trash-cli")
   
   (specification->package "virt-manager")
   (specification->package "dconf")
   (specification->package "gtksourceview@4")
   (specification->package "virt-viewer")
   (specification->package "spice-vdagent")
   
   (specification->package "xev")
   (specification->package "xset")
   (specification->package "xrdb")
   (specification->package "xhost")
   (specification->package "xmodmap")
   (specification->package "setxkbmap")
   (specification->package "xss-lock")
   (specification->package "libinput")
   (specification->package "xinput")
   
   ))
