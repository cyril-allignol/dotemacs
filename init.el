;; Restore default GC and filename handler
(add-hook 'after-init-hook
          (lambda nil
            (setq gc-cons-threshold  16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist old-file-name-handler)))

(load "~/.emacs.d/local")

;; I don't use system packages, and manage emacs-specific packages with
;; use-package. First set-up GNU and MELPA repositories and initialize
;; built-in package manager:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

;; Then install use-package if necessary, and use it:
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Now read configuration file
(org-babel-load-file "~/.emacs.d/config.org")
