;; early-init.el -- keep startup minimal and avoid package.el auto-init
(setq package-enable-at-startup nil)
(setq inhibit-default-init t)
;; A tiny performance nudge for startup
(setq gc-cons-threshold (* 64 1024 1024))
