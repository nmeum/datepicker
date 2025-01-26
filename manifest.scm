(use-modules (guix packages)
             (guix gexp)
             (guix download)
             (guix build-system haskell)
             ((guix licenses) #:prefix license:)

             (gnu packages tmux)
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
    (inputs (list ghc-random
                  ghc-tasty
                  ghc-tasty-hunit
                  ghc-tasty-quickcheck))
    (home-page "https://github.com/haskell/time")
    (synopsis "A time library for Haskell")
    (description "")
    (license license:bsd-2)))

(define-public ghc-tasty-tmux
  (package
    (name "ghc-tasty-tmux")
    (version "0.1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "tasty-tmux" version))
       (sha256
        (base32 "12v1avr74zsaq2dnssbip7pc6qalh8w7ilqb5z8azplg6h83vh93"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "tasty-tmux")))
    (propagated-inputs (list tmux))
    (inputs (list ghc-typed-process
                  ghc-regex-posix
                  ghc-tasty
                  ghc-tasty-hunit))
    (home-page "https://github.com/purebred-mua/tasty-tmux")
    (synopsis "Terminal user acceptance testing (UAT) via tmux")
    (description "")
    (license license:agpl3)))

(package
  (name "datepicker")
  (version "0.1.0")
  (source (local-file "." "git-checkout"
                      #:recursive? #t))
  (inputs
    (list
      ghc-9.2))
  (native-inputs
    (list ghc-time
          ghc-vty
          ghc-vty-unix
          ghc-optparse-applicative

          ghc-tasty
          ghc-tasty-tmux
          ghc-tasty-hunit
          ghc-tasty-quickcheck))
  (build-system haskell-build-system)
  (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'check 'setenv-check
          (lambda _
            (setenv "PATH" (string-append (getcwd) "/dist/build/datepicker:"
                                          (getenv "PATH"))))))))
  (synopsis "An fzf-like tool to interactively select a date in a provided format")
  (description "")
  (home-page "https://git.8pit.net/datepicker")
  (license license:gpl3+))
