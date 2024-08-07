(define-module (pg home services media)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages video)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:export (home-media-service-type))

(define (home-media-profile-service config)
  (list ffmpeg
        gstreamer
        gst-libav
        gst-plugins-bad
        gst-plugins-base
        gst-plugins-good
        gst-plugins-ugly
        intel-vaapi-driver
        libva-utils
        mpd-mpc
        mpv
        mpv-mpris
        obs
        obs-pipewire-audio-capture
        obs-wlrobs
        pavucontrol
        pipewire
        playerctl
        wireplumber
        youtube-dl))

(define (home-pipewire-shepherd-service config)
  (shepherd-service
   (documentation "Runs `pipewire'")
   (provision '(pipewire))
   (requirement '(dbus))
   (start #~(make-forkexec-constructor
             (list #$(file-append pipewire "/bin/pipewire"))))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-pipewire-pulseaudio-shepherd-service config)
  (shepherd-service
   (documentation "Runs pulseaudio replacement service for pipewire")
   (provision '(pipewire-pulseaudio))
   (requirement '(pipewire))
   (start #~(make-forkexec-constructor
             (list #$(file-append pipewire "/bin/pipewire-pulse"))))
   (stop #~(make-kill-destructor))))

(define (home-wireplumber-shepherd-service config)
  (shepherd-service
   (documentation "Runs `wireplumber' session management for pipewire")
   (provision '(wireplumber))
   (requirement '(pipewire))
   (start #~(make-forkexec-constructor
             (list #$(file-append wireplumber "/bin/wireplumber"))))
   (stop #~(make-kill-destructor))))

(define (home-playerctld-shepherd-service config)
  (shepherd-service
   (documentation "Runs `playerctld'")
   (provision '(playerctld))
   (requirement '(dbus))
   (start #~(make-forkexec-constructor
             (list #$(file-append playerctl "/bin/playerctld"))))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-mpd-shepherd-service config)
  (shepherd-service
   (documentation "Runs `mpd'")
   (provision '(mpd))
   (start #~(make-forkexec-constructor
             (list #$(file-append mpd "/bin/mpd") "--no-daemon")))
   (stop #~(make-kill-destructor))
   (respawn? #f)))

(define (home-mpDris2-shepherd-service config)
  (shepherd-service
   (documentation "Runs `mpDris2'")
   (provision '(mpDris2))
   (requirement '(mpd))
   (start #~(make-forkexec-constructor
             (list #$(file-append mpdris2 "/bin/mpDris2"))))
   (stop #~(make-kill-destructor))))

(define (home-media-shepherd-services config)
  (list (home-pipewire-shepherd-service config)
        (home-pipewire-pulseaudio-shepherd-service config)
        (home-wireplumber-shepherd-service config)
        (home-playerctld-shepherd-service config)
        (home-mpd-shepherd-service config)
        (home-mpDris2-shepherd-service config)))

(define home-pipewire-asoundrc
  (mixed-text-file
   "asoundrc"
   "<" pipewire "/share/alsa/alsa.conf.d/50-pipewire.conf>\n"
   "<" pipewire "/share/alsa/alsa.conf.d/99-pipewire-default.conf>\n"
   "pcm_type.pipewire {\n"
   "  lib \"" pipewire "/lib/alsa-lib/libasound_module_pcm_pipewire.so\"\n"
   "}\n"
   "ctl_type.pipewire {\n"
   "  lib \"" pipewire "/lib/alsa-lib/libasound_module_ctl_pipewire.so\"\n"
   "}\n"))

(define home-pipewire-disable-pulseaudio-auto-start
  (plain-file "client.conf" "autospawn = no"))

(define (home-media-xdg-configuration config)
  (list `("alsa/asoundrc" ,home-pipewire-asoundrc)
        `("pulse/client.conf" ,home-pipewire-disable-pulseaudio-auto-start)))

(define home-media-service-type
  (service-type (name 'home-audio)
                (description "Service for various media utilities")
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-media-profile-service)
                       (service-extension home-shepherd-service-type
                                          home-media-shepherd-services)
                       (service-extension home-xdg-configuration-files-service-type
                                          home-media-xdg-configuration)))
                (default-value #f)))
