(in-package #:nyxt-user)

(define-configuration window
    ((message-buffer-style
      (str:concat
       %slot-default%
       (cl-css:css
        '((body
           :background-color "#282c34"
           :color "#cccccc")))))))
;;; Color config for prompt-buffer (minibuffer in Emacs parlance).
(define-configuration prompt-buffer
    ((style (str:concat
             %slot-default%
             (cl-css:css
              '((body
                 :background-color "#282c34"
                 :color "#cccccc")
                ("#prompt-area"
                 :background-color "#282c34")
                ;; The area you input text in.
                ("#input"
                 :background-color "#cccccc")
                (".source-name"
                 :color "#cccccc"
                 :background-color "#202231")
                (".source-content"
                 :background-color "#282c34")
                (".source-content th"
                 :border "1px solid #202231"
                 :background-color "#282c34")
                ;; The currently highlighted option.
                ("#selection"
                 :background-color "#98f979"
                 :color "#282c34")
                (.marked :background-color "#4682d9"
                         :font-weight "bold"
                         :color "#cccccc")
                (.selected :background-color "#282c34"
                           :color "#cccccc")))))))
;;; Internal (i.e. help, info, describe-* buffers). Usually work for
;;; simple HTML display, so I'm overriding lots of things there.
;;;
;;; Panel buffers are the same in regards to style.
(define-configuration (internal-buffer panel-buffer)
    ((style
      (str:concat
       %slot-default%
       (cl-css:css
        '((title
           :color "#cccccc")
          (body
           :background-color "#282c34"
           :color "#cccccc")
          (hr
           :color "#eead0e")
          (a
           :color "#61afef")
          (.button
           :color "#ffffff"
           :background-color "#006fff")))))))
;;; History-tree-mode is a mode used in `history-tree' and
;;; `buffer-history-tree' buffers. It's not enough to customize
;;; `internal-buffer' to cover it, thus I'm customizing it
;;; specifically.
(define-configuration nyxt/history-tree-mode:history-tree-mode
    ((nyxt/history-tree-mode::style
      (str:concat
       %slot-default%
       (cl-css:css
        '((body
           :background-color "#282c34"
           :color "#cccccc")
          (hr
           :color "#cccccc")
          (a
           :color "#61afef")
          ;; Those three below are here to color the tree-branching list
          ;; markers in white.
          ("ul li::before"
           :background-color "#cccccc")
          ("ul li::after"
           :background-color "#cccccc")
          ("ul li:only-child::before"
           :background-color "#cccccc")))))))
(define-configuration nyxt/web-mode:web-mode
    ;; The style of highlighted boxes, e.g. link hints.
    ((nyxt/web-mode:highlighted-box-style
      (cl-css:css
       '((".nyxt-hint.nyxt-highlight-hint"
          :font-weight "normal"
          :background "#494949"))))))
;;; Status buffer is the strip above the message buffer/echo area.
;;; Modeline in Emacs parlance.
(define-configuration status-buffer
    ((style (str:concat
             %slot-default%
             (cl-css:css
            ;; Arrows on the left.
              '(("#controls"
                 :border-top "1px solid #ffffff"
                 :background-color "#737373")
                ;; To the right of the arrows.
                ("#url"
                 :background-color "#21252b"
                 :color "#cccccc"
                 :border-top "1px solid #cccccc")
                ;; Far to the right.
                ("#modes"
                 :background-color "#21252b"
                 :border-top "1px solid #cccccc")
                ;; The center segment.
                ("#tabs"
                 :background-color "#737373"
                 :color "#282c34"
                 :border-top "1px solid #cccccc")))))))
(define-configuration nyxt/style-mode:dark-mode
    ((style #.(cl-css:css
               '((*
                  :background-color "#282c34 !important"
                  :background-image "none !important"
                  :color "#cccccc")
                 (a
                  :background-color "#282c34 !important"
                  :background-image "none !important"
                  :color "#556B2F !important"))))))
