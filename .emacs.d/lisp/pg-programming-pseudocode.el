;;; pg-programming-pseudocode.el -*- lexical-binding: t; -*-

(straight-use-package '(pseudocode-mode :type git
                                        :host github
                                        :repo "jsalzbergedu/pseudocode-mode"))

(add-to-list 'auto-mode-alist '("\\.psc\\'" . pseudocode-mode))

(provide 'pg-programming-pseudocode)
