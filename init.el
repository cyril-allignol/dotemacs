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
