(use-modules (guix packages)
             (guix gexp)
             (guix download)
             (guix build-system haskell)
             ((guix licenses) #:prefix license:)

             (gnu packages haskell)
             (gnu packages haskell-check)
             (gnu packages haskell-xyz))

;; The version of the time library shipped by ghc-9.2 is too old.
(define-public ghc-time
  (package
    (name "ghc-time")
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "time" version))
       (sha256
        (base32 "0gkzffnvi33ksw4zln0d31dpmqiyl8gicrx04g8j13kjr5ygx86z"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "time")))
    (native-inputs (list ghc-random
                         ghc-quickcheck
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck
                         ghc-random
                         ghc-quickcheck
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/haskell/time")
    (synopsis "A time library for Haskell")
    (description "")
    (license license:bsd-2)))

(package
  (name "datepicker")
  (version "0.1.0.0")
  (source (local-file "." "git-checkout"
                      #:recursive? #t))
  (build-system haskell-build-system)
  (inputs
    (list
      ghc-9.2))
  (native-inputs
    (list
      ghc-time
      ghc-vty
      ghc-vty-unix
      ghc-optparse-applicative))
  (synopsis "An fzf-like tool to interactively select a date in a provided format")
  (description "")
  (home-page "https://git.8pit.net/datepicker")
  (license license:gpl3+))
