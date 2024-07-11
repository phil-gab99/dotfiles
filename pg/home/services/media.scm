(define-module (pg home services media)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (home-media-service-type))

(use-package-modules linux mpd music pulseaudio video)
(use-service-modules shepherd)

(define (home-media-profile-service config)
  (list ffmpeg
        mpdris2
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
   (stop #~(make-kill-destructor))))

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
   (start #~(make-forkexec-constructor
             (list #$(file-append playerctl "/bin/playerctld"))))
   (stop #~(make-kill-destructor))))

(define (home-mpDris2-shepherd-service config)
  (shepherd-service
   (documentation "Runs `mpDris2'")
   (provision '(mpDris2))
   (start #~(make-forkexec-constructor
             (list #$(file-append mpdris2 "/bin/mpDris2"))))
   (stop #~(make-kill-destructor))))

(define (home-media-shepherd-services config)
  (list (home-pipewire-shepherd-service config)
        (home-pipewire-pulseaudio-shepherd-service config)
        (home-wireplumber-shepherd-service config)
        (home-playerctld-shepherd-service config)
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
