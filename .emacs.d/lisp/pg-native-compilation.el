;;; pg-native-compilation.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(when (featurep 'native-compile)
  (setopt native-comp-async-report-warnings-errors nil)                                          ;; Silence compiler warnings
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))) ;; Set directory for cache storage

(provide 'pg-native-compilation)
