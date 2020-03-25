;; Allow a larger GC space at startup, then garbage-collect and reduce
;; this amount (taken from
;; [[https://github.com/jwiegley/dot-emacs/blob/master/init.el][here]]):
(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

(load-file "~/.emacs.d/local.el")

;; I don't use system packages, and manage emacs-specific packages with
;; use-package. First set-up GNU and MELPA repositories and initialize
;; built-in package manager:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Then install use-package if necessary, and use it:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Now read configuration file
(org-babel-load-file "~/.emacs.d/config.org")
