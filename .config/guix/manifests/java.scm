(use-modules
 (guix inferior)
 (guix channels)
 (srfi srfi-1))

(define channels
  (list
   (channel (name 'guix)
            (url "https://git.savannah.gnu.org/git/guix.git")
            (commit "d039f9dc151eed8017a7f54682dbf713221b8005"))))

(define inferior
  (inferior-for-channels channels))

(packages->manifest
 (list
  (specification->package+output "openjdk@17:jdk")
  (specification->package+output "openjdk@17:doc")
  (first (lookup-inferior-packages inferior "maven"))))
