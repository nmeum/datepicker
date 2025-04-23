(use-modules ((guix ui) #:select (load* make-user-module))
             ((guix profiles) #:select (concatenate-manifests)))

;; TODO: Doesn't fully work yet because `cabal build` employs stricter
;; dependency checking in comparison to whatever haskell-build-system
;; uses; hence, cabal discovers that ghc-time-1.14 is incompatible with
;; the version of the unix library vendored by ghc-9.2.
;;
;; All of this will resolve itself once ghc-9.4 is stabilized in Guix.

(define datepicker
  (load* "package.scm" (make-user-module '())))

(concatenate-manifests
  (list (package->development-manifest datepicker)
        (specifications->manifest
          ;; Extra development dependencies.
          ;;
          ;; TODO: Add ormolu
          (list "coreutils" "cabal-install" "git"))))
