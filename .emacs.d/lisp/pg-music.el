(defun pg/start-mpd ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "herd start mpd")
  (pg/update-mpd-db)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (emms-smart-browse)
  (message "MPD Started!"))

(defun pg/kill-mpd ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (emms-smart-browse)
  (emms-player-mpd-disconnect)
  (shell-command "herd stop mpd")
  (message "MPD Killed!"))

(defun pg/update-mpd-db ()
  "Updates the MPD database synchronously."
  (interactive)
  (pg/call-mpc nil "update")
  (message "MPD Database Updated!"))

(defun pg/convert-number-to-relative-string (number)
  "Convert an integer NUMBER to a prefixed string.

      The prefix is either - or +. This is useful for mpc commands
      like volume and seek."
  (let ((number-string (number-to-string number)))
    (if (> number 0)
        (concat "+" number-string)
      number-string)))

(defun pg/call-mpc (destination mpc-args)
  "Call mpc with `call-process'.

      DESTINATION will be passed to `call-process' and MPC-ARGS will be
      passed to the mpc program."
  (if (not (listp mpc-args))
      (setq mpc-args (list mpc-args)))
  (apply 'call-process "mpc" nil destination nil mpc-args))

(defun pg/message-current-volume ()
  "Return the current volume."
  (message "%s"
           (with-temp-buffer
             (pg/call-mpc t "volume")
             (delete-char -1)  ;; delete trailing \n
             (buffer-string))))

(defun pg/emms-volume-amixer-change (amount)
  "Change amixer master volume by AMOUNT."
  (let ((volume-change-string (pg/convert-number-to-relative-string amount)))
    (pg/call-mpc nil (list "volume" volume-change-string)))
  (pg/message-current-volume))

(straight-use-package 'emms)
(unless pg/is-termux
  (require 'emms)
  (with-eval-after-load 'emms
    (require 'emms-setup)
    (require 'emms-player-mpd)
    (emms-all)
    (setq emms-info-functions '(emms-info-mpd)
          emms-player-list '(emms-player-mpd))
    (add-hook 'emms-playlist-cleared-hook #'emms-player-mpd-clear)
    (fset #'emms-volume-amixer-change #'pg/emms-volume-amixer-change)

    ;;      (bind-keys :package emms
    ;;		 ("<XF86AudioPrev>" . emms-previous)
    ;;		 ("<XF86AudioNext>" . emms-next)
    ;;		 ("<XF86AudioPlay>" . emms-pause)
    ;;		 ("<XF86AudioStop>" . emms-stop))
    (customize-set-variable 'emms-source-file-default-directory "/home/phil-gab99/Music")
    (customize-set-variable 'emms-player-mpd-music-directory "/home/phil-gab99/Music")
    (customize-set-variable 'emms-seek-seconds 5)
    (customize-set-variable 'emms-volume-change-amount 5)))

(straight-use-package 'emms-mode-line-cycle)
(with-eval-after-load 'emms
  (require 'emms-mode-line-cycle)
  (with-eval-after-load 'emms-mode-line-cycle
    (emms-mode-line 1)
    (emms-playing-time 1)
    (require 'emms-mode-line-icon)
    (customize-set-variable 'emms-mode-line-cycle-use-icon-p t)
    (emms-mode-line-cycle 1)))

(provide 'pg-music)
