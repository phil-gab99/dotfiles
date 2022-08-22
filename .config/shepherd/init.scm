(use-modules ((ice-9 ftw) #:select (scandir)))

;; Run shepherd in background
(action 'shepherd 'daemonize)

;; Load all the files in the directory 'init.d' with a suffix '.scm'.
(for-each
 (lambda (file)
   (load (string-append "init.d/" file)))
 (scandir (string-append (dirname (current-filename)) "/init.d")
          (lambda (file)
            (string-suffix? ".scm" file))))
