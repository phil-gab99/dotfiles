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

(straight-use-package 'emms)
(unless (fboundp 'emms-smart-browse)
  (autoload #'emms-smart-browse "emms" nil t))
(with-eval-after-load 'emms
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (add-hook 'emms-playlist-cleared #'emms-player-mpd-clear)
  (pg/customize-set-variables
   `((emms-source-file-default-directory . ,(expand-file-name "~/Music"))
     (emms-player-mpd-music-directory . ,(expand-file-name "~/Music"))
     (emms-player-list . (emms-player-mpd))
     (emms-volume-change-function ,#'emms-volume-mpd-change)))
  (emms-all)
  (emms-default-players)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (dolist (binding `((,(kbd "<XF86AudioPrev>") . ,#'emms-previous)
                     (,(kbd "<XF86AudioNext>") . ,#'emms-next)
                     (,(kbd "<XF86AudioPlay>") . ,#'emms-pause)
                     (,(kbd "<XF86AudioStop>") . ,#'emms-stop)))
    (define-key emms-browser-mode-map (car binding) (cdr binding))))

(defun pg/emms-mode-line-cycle--icon-function (&optional title initialp)
  "Format the current track TITLE like `emms-mode-line-icon-function'. If
  INITIALP is no-nil, initialized."
  (concat " "
          emms-mode-line-icon-before-format
          ;; (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
          (emms-mode-line-cycle--playlist-current title initialp)))

(straight-use-package 'emms-mode-line-cycle)
(with-eval-after-load 'emms
  (require 'emms-mode-line-cycle))
(with-eval-after-load 'emms-mode-line-cycle
  (require 'emms-mode-line-icon)
  (fset #'emms-mode-line-cycle--icon-function #'pg/emms-mode-line-cycle--icon-function)
  (customize-set-variable 'emms-mode-line-cycle-use-icon-p t)
  (emms-mode-line 1)
  (emms-playing-time 1)
  (emms-mode-line-cycle 1))

(straight-use-package 'simple-mpc)
(unless (fboundp 'simple-mpc)
  (autoload #'simple-mpc "simple-mpc" nil t))
(with-eval-after-load 'simple-mpc
  (define-key simple-mpc-mode-map (kbd "<XF86AudioPlay>") #'simple-mpc-toggle))

(provide 'pg-music)
