(use-modules (gnu)
             (guix profiles))

(use-package-modules databases)

(packages->manifest
 (list postgresql
       sqls))
