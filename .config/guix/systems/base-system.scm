(define-module (base-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system nss)
  #:use-module (gnu services pm)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-service-modules nix)
(use-service-modules desktop xorg)
(use-package-modules certs)
(use-package-modules shells)

;; Allow members of the "video" group to change the screen brightness.
;  (define %backlight-udev-rule
;    (udev-rule
;     "90-backlight.rules"
;     (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
;		    "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
;		    "\n"
;		    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
;		    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

;  (define %my-desktop-services
;    (modify-services %desktop-services
;		     (elogind-service-type config =>
;					   (elogind-configuration (inherit config)
;								  (handle-lid-switch-external-power 'suspend)))
;		     (udev-service-type config =>
;					(udev-configuration (inherit config)
;							    (rules (cons %backlight-udev-rule
;									 (udev-configuration-rules config)))))
;		     (network-manager-service-type config =>
;						   (network-manager-configuration (inherit config)
;										  (vpn-plugins (list network-manager-openvpn))))))

(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")
