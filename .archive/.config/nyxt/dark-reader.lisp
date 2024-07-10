(in-package #:nyxt-user)

(define-configuration nx-dark-reader:dark-reader-mode
  ((nxdr:selection-color "#494949")
   (nxdr:background-color "#282c34")
   (nxdr:text-color "#cccccc")))

(push 'nx-dark-reader:dark-reader-mode *web-buffer-modes*)
