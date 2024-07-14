(define-module (pg home services wayland)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (nongnu packages compression)
  #:export (home-wayland-service-type))

(use-package-modules admin compression disk fonts freedesktop glib gnome
                     gnome-xyz imagemagick kde-frameworks libcanberra
                     libreoffice package-management password-utils python-build
                     qt rust-apps shellutils terminals video virtualization
                     web-browsers wm xdisorg xorg)
(use-service-modules shepherd)

(define (home-wayland-profile-service config)
  (list sway
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

        ;; Authentication
        password-store

        ;; General CLI utilities
        imagemagick
        ripgrep
        p7zip
        unrar
        wofi
        tree

        ;; Basic Applications
        wlogout
        libreoffice
        evince
        simple-scan
        virt-manager))

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
   (stop #~(make-kill-destructor))))

(define (home-swayidle-shepherd-service config)
  (shepherd-service
   (provision '(swayidle))
   (requirement '(dbus))
   (documentation "Runs `swayidle'")
   (auto-start? #f)
   (start (let ((cmd-swayidle (file-append swayidle "/bin/swayidle"))
                (cmd-swaylock (file-append swaylock "/bin/swaylock"))
                (cmd-swaymsg (file-append sway "/bin/swaymsg")))
            #~(make-forkexec-constructor
               (list #$cmd-swayidle "-w"
                     ;; "timeout" "300" #$cmd-swaylock
                     "timeout" "300"
                     (string-append "'" #$cmd-swaymsg " \"output * power off\"'")
                     "resume"
                     (string-append "'" #$cmd-swaymsg " \"output * power on\"'")
                     "before-sleep" #$cmd-swaylock)
               #:log-file
               (string-append (or (getenv "XDG_STATE_HOME")
                                  (format #f "~a/.local/state"
                                          (getenv "HOME")))
                              "/shepherd/swayidle.log")
               #:environment-variables
               (cons "WAYLAND_DISPLAY=wayland-1"
                     (default-environment-variables)))))
   (stop #~(make-kill-destructor))))

(define (home-wayland-shepherd-services config)
  (list (home-mako-shepherd-service config)
        (home-swayidle-shepherd-service config)))

(define home-wayland-service-type
  (service-type (name 'home-wayland)
                (description "Wayland environment service")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-wayland-profile-service)
                       (service-extension home-shepherd-service-type
                                          home-wayland-shepherd-services)))
                (default-value #f)))
