(use-modules (gnu packages game-development)
             (gnu packages gimp)
             (guix profiles))

(packages->manifest
 (list gimp
       godot))
