(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "27fc4ae397f068fc21943e86315a71b42305f730")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "399b277e82dd7115a25bcaabaf58f6dd40ebf920"))
      (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (branch "master")
        (commit
          "c5f3671d2e45d76d0318745762c56753a334c600")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
        (name 'flat)
        (url "https://github.com/flatwhatson/guix-channel.git")
        (branch "master")
        (commit
          "094746c1e2e90f2df1e598ab1fd0abb4d75ce84d")
        (introduction
          (make-channel-introduction
            "33f86a4b48205c0dc19d7c036c85393f0766f806"
            (openpgp-fingerprint
              "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490")))))
