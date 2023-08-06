(use-modules
 (shepherd service)
 ((ice-9 ftw) #:select (scandir)))

;; Run shepherd in background
(perform-service-action (lookup-running 'shepherd) 'daemonize)

;; Load all the files in the directory 'init.d' with a suffix '.scm'.
(for-each
 (lambda (file)
   (load (string-append "init.d/" file)))
 (scandir (string-append (dirname (current-filename)) "/init.d")
          (lambda (file)
            (string-suffix? ".scm" file))))

;; Register services
(register-services (list autorandr
                         compton
                         dunst
                         feh
                         gpg-agent
                         mcron
                         mpd
                         nm-applet
                         pasystray
                         polkit-gnome
                         pulseaudio
                         syncthing
                         syncthing-gtk
                         udiskie
                         xmodmap
                         xsettingsd
                         xss-lock))

;; Start services
(start-in-the-background '(gpg-agent
                           mcron
                           polkit-gnome
                           pulseaudio
                           syncthing
                           syncthing-gtk
                           xmodmap
                           xsettingsd))
