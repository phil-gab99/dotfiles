;;; pg-music.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

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

(defun pg/call-mpc (destination mpc-args)
  "Call mpc with `call-process'.

      DESTINATION will be passed to `call-process' and MPC-ARGS will be
      passed to the mpc program."
  (if (not (listp mpc-args))
      (setq mpc-args (list mpc-args)))
  (apply 'call-process "mpc" nil destination nil mpc-args))

(use-package emms
  :straight t
  :init
  (require 'emms)
  (require 'emms-setup)
  (require 'emms-player-mpd)
  :hook
  (emms-playlist-cleared . emms-player-mpd-clear)
  :custom
  (emms-player-mpd-music-directory "/home/phil-gab99/Music")
  (emms-player-list '(emms-player-mpd))
  (emms-volume-change-function #'emms-volume-mpd-change)
  :bind
  (:map emms-browser-mode-map
        ("<XF86AudioPrev>" . emms-previous)
        ("<XF86AudioNext>" . emms-next)
        ("<XF86AudioPlay>" . emms-pause)
        ("<XF86AudioStop>" . emms-stop))
  :config
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (emms-all))

(defun pg/emms-mode-line-cycle--icon-function (&optional title initialp)
  "Format the current track TITLE like `emms-mode-line-icon-function'.
If INITIALP is no-nil, initialized."
  (concat " "
          emms-mode-line-icon-before-format
          ;; (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-cycle--playlist-current title initialp)))

(use-package emms-mode-line-cycle
  :straight t
  :init
  (require 'emms-mode-line-cycle)
  (require 'emms-mode-line-icon)
  (fset #'emms-mode-line-cycle--icon-function #'pg/emms-mode-line-cycle--icon-function)
  :after emms
  :custom
  (emms-mode-line-cycle-use-icon-p t)
  :config
  (emms-mode-line 1)
  (emms-playing-time 1)
  (emms-mode-line-cycle 1))

(provide 'pg-music)
