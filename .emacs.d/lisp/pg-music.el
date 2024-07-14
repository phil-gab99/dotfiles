;;; pg-music.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'emms)
(unless (fboundp 'emms-smart-browse)
  (autoload #'emms-smart-browse "emms" nil t))
(with-eval-after-load 'emms
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (add-hook 'emms-playlist-cleared #'emms-player-mpd-clear)
  (setopt emms-source-file-default-directory (plist-get pg/user :music)
          emms-player-mpd-music-directory (plist-get pg/user :music)
          emms-player-list '(emms-player-mpd)
          emms-volume-change-function #'emms-volume-mpd-change)
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
  (setopt emms-mode-line-cycle-use-icon-p t)
  (emms-mode-line 1)
  (emms-playing-time 1)
  (emms-mode-line-cycle 1))

(provide 'pg-music)
