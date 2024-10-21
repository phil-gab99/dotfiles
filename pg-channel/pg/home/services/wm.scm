(define-module (pg home services wm)
  #:use-module (gnu)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages compression)
  #:export (home-wm-service-type))

(use-package-modules admin compression databases fonts freedesktop glib gnome
                     gnome-xyz gtk kde-frameworks libcanberra libreoffice
                     package-management password-utils python-build qt rust-apps
                     shellutils terminals video web-browsers wm xdisorg xorg)
(use-service-modules shepherd)

(define (home-wm-profile-service config)
  (list sway
        swayidle
        swaylock

        ;; Top bar
        waybar

        ;; Notifications
        mako
        libnotify

        ;; App Launcher
        fuzzel

        ;; Screenshot
        grimshot

        ;; Themes and fonts
        adwaita-icon-theme
        breeze-icons
        font-awesome
        font-google-noto
        font-google-noto-emoji
        font-iosevka-aile
        font-jetbrains-mono
        font-liberation
        gnome-themes-extra
        matcha-theme
        papirus-icon-theme
        sound-theme-freedesktop

        ;; Terminal emulator
        foot

        ;; Compatibility for older Xorg applications
        xorg-server-xwayland

        ;; XDG utilities
        xdg-desktop-portal
        xdg-desktop-portal-gtk
        xdg-desktop-portal-wlr
        xdg-utils
        xdg-dbus-proxy
        shared-mime-info
        (list gtk+ "bin")
        (list glib "bin")
        v4l-utils

        ;; Package managers
        conda
        (specification->package "node@18")
        python-pip
        direnv

        ;; Browsers
        qtwayland
        qutebrowser

        ;; VMs
        gnome-boxes

        ;; Authentication
        password-store

        ;; General CLI utilities
        ripgrep
        p7zip
        unrar
        wofi
        tree
        recutils

        ;; Basic Applications
        wlogout
        libreoffice
        evince
        simple-scan))

(define (home-gammastep-shepherd-service config)
  (shepherd-service
   (provision '(gammastep))
   (documentation "Run `gammastep-indicator'")
   (auto-start? #f)
   (start #~(make-forkexec-constructor
             (list #$(file-append gammastep "/bin/gammastep-indicator"))
             #:environment-variables
             (cons "WAYLAND_DISPLAY=wayland-1"
                   (default-environment-variables))))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-mako-shepherd-service config)
  (shepherd-service
   (provision '(mako))
   (requirement '(dbus))
   (documentation "Runs `mako'")
   (auto-start? #f)
   (start #~(make-forkexec-constructor
             (list #$(file-append mako "/bin/mako"))
             #:environment-variables
             (cons "WAYLAND_DISPLAY=wayland-1"
                   (default-environment-variables))))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-nm-applet-shepherd-service config)
  (shepherd-service
   (provision '(nm-applet))
   (documentation "Run `nm-applet'")
   (auto-start? #f)
   (start #~(make-forkexec-constructor
             (list #$(file-append network-manager-applet "/bin/nm-applet"))
             #:environment-variables
             (cons "WAYLAND_DISPLAY=wayland-1"
                   (default-environment-variables))))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-udiskie-shepherd-service config)
  (shepherd-service
   (provision '(udiskie))
   (requirement '(dbus))
   (documentation "Run `udiskie'")
   (auto-start? #f)
   (start #~(make-forkexec-constructor
             (list #$(file-append udiskie "/bin/udiskie") "-t")
             #:environment-variables
             (cons "WAYLAND_DISPLAY=wayland-1"
                   (default-environment-variables))))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-wm-shepherd-services config)
  (list (home-gammastep-shepherd-service config)
        (home-mako-shepherd-service config)
        (home-nm-applet-shepherd-service config)
        (home-udiskie-shepherd-service config)))

(define home-wm-service-type
  (service-type (name 'home-wm)
                (description "Wayland environment service")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-wm-profile-service)
                       (service-extension home-shepherd-service-type
                                          home-wm-shepherd-services)))
                (default-value #f)))
