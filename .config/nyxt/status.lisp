(in-package #:nyxt-user)

;;; Display modes as short glyphs (listed below) in the mode line
;;; (bottom-right of the screen).
(define-configuration status-buffer
  ((glyph-mode-presentation-p t)))

(define-configuration nyxt/force-https-mode:force-https-mode ((glyph "ϕ")))
(define-configuration nyxt/blocker-mode:blocker-mode ((glyph "β")))
(define-configuration nyxt/proxy-mode:proxy-mode ((glyph "π")))
(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode
  ((glyph "τ")))
(define-configuration nyxt/certificate-exception-mode:certificate-exception-mode
  ((glyph "χ")))
(define-configuration nyxt/style-mode:style-mode ((glyph "ϕ")))
(define-configuration nyxt/help-mode:help-mode ((glyph "?")))
(define-configuration nyxt/web-mode:web-mode ((glyph "ω")))
(define-configuration nyxt/auto-mode:auto-mode ((glyph "α")))
(define-configuration nyxt/cruise-control-mode:cruise-control-mode ((glyph "σ")))
