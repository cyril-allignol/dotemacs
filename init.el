;;=======================================
;;               EMACS PATH
;;=======================================
(setq gc-cons-threshold 100000000)
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))
;(setq explicit-bash-args '("--login" "-i"))

(server-start)

(require 'local nil t)

;;=======================================
;;              PACKAGES
;;=======================================
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :defer t
  :ensure t)
;; (require 'bind-key)

;;=======================================
;;              GLOBAL FLAGS
;;=======================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#212121" "#B71C1C" "#558b2f" "#FFA000" "#2196f3" "#4527A0" "#00796b" "#FAFAFA"))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("4182c491b5cc235ba5f27d3c1804fc9f11f51bf56fb6d961f94788be034179ad" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(fci-rule-color "#ECEFF1")
 '(hl-sexp-background-color "#efebe9")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(lpr-command "lpr -P lfsb")
 '(magit-diff-use-overlays nil)
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

(add-to-list 'default-frame-alist '(width . 81))

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)

;; Backup file settings
(setq
   backup-by-copying t      ; don't clobber symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
;; Save by default in ~/.saves folder
(push (cons "." "~/.saves") backup-directory-alist)

;;=======================================
;;             APPEARANCE
;;=======================================
(blink-cursor-mode 0)
(menu-bar-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "")

(use-package all-the-icons
  :defer t
  :ensure t)

(use-package neotree
  :defer t
  :ensure t
  :bind ([f3] . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/mode-width "full"
        sml/name-width 40)
  (rich-minority-mode 1)
  (setf rm-blacklist "")
  (setq sml/theme 'respectful)
  (sml/setup)
  :config
  (setq sml/shorten-directory t
        sml/shorten-modes t)
  (add-to-list 'sml/replacer-regexp-list '("^~/python/" ":PY:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/latex/" ":TeX:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/ownCloud/partake" ":PTK:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/ownCloud/" ":OC:") t)
  )

;; Theme cycling
;; taken from:
;; https://github.com/habamax/.emacs.d/blob/master/lisp/haba-appearance.el

;; (defvar *my-theme-dark* 'tango-dark)
;;(defvar *my-theme-light* 'leuven)
;; (defvar *my-theme-dark* 'material)
(defvar *my-theme-light* 'material-light)
(defvar *my-theme-dark* 'doom-one)
(defvar *my-current-theme* *my-theme-dark*)

(load-theme *my-theme-dark*)

;; disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapcar #'disable-theme custom-enabled-themes))

(defun my/next-theme (theme)
  (if (eq theme 'default)
      (disable-theme *my-current-theme*)
    (progn
      (load-theme theme t)))
  (setq *my-current-theme* theme))

(defun my/toggle-theme ()
  (interactive)
  (cond
   ((eq *my-current-theme* *my-theme-dark*) (my/next-theme *my-theme-light*))
   ((eq *my-current-theme* *my-theme-light*) (my/next-theme *my-theme-dark*))))

;; from: http://www.whiz.se/2016/05/01/dark-theme-in-emacs/
;; (defun set-dark-wm-theme (frame)
;;   (select-frame frame) ;; this is important!
;;   (when (display-graphic-p)
;;     (progn
;;       (when (file-exists-p "/usr/bin/xprop")
;;     (progn
;;       (defvar winid nil)
;;       (setq winid (frame-parameter frame 'outer-window-id))
;;       (call-process "xprop" nil nil nil "-f" "_GTK_THEME_VARIANT" "8u" "-set" "_GTK_THEME_VARIANT" "dark" "-id" winid))))))

;; (defun dark-frame ()
;;   (interactive)
;;   (set-dark-wm-theme (selected-frame))
;;   (unless (display-graphic-p (selected-frame))
;;     (set-face-background 'default "unspecified-bg" (selected-frame))))

;; (add-hook 'window-setup-hook 'dark-frame)
;; (add-hook 'after-make-frame-functions 'set-dark-wm-theme)

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-margin 5
      hscroll-step 5)

;;=======================================
;;                  KEYS
;;=======================================
;; (define-key global-map [f1]   'undo)
;; (define-key global-map [f2]   'vc-toggle-read-only)
;; (define-key global-map [f3]   'iso-accents-mode)
(define-key global-map [f4]  'goto-char)
(define-key global-map [f5]  'goto-line)
(define-key global-map [f6]  'compile)
(define-key global-map [f7]  'camldebug)
(define-key global-map [f8]  'next-error)
(define-key global-map [shift f8]  'previous-error)
(define-key global-map [f10] 'my/toggle-theme)
(use-package writeroom
  :defer t
  :ensure writeroom-mode
  :bind ([f12] . writeroom-mode))
(define-key global-map [?²]  'dabbrev-expand)
(define-key global-map (kbd "C-x C-b") 'electric-buffer-list)

;;=======================================
;;           AUTO-COMPLETION
;;=======================================
(use-package auto-complete
  :defer t
  :ensure t
  :init (setq ac-auto-start nil)
  :bind (:map ac-mode-map ("<C-tab>" . auto-complete)))

;;=======================================
;;             MULTI-CURSOR
;;=======================================
(use-package multiple-cursors
  :defer t
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-* C-*" . mc/mark-all-like-this)
         ("C-* C-a" . mc/edit-beginnings-of-lines)
         ("C-* C-e" . mc/edit-ends-of-lines)
         ("C-* C-i" . mc/insert-numbers)))

;;=======================================
;                HISTORIQUE
;;=======================================
(use-package savehist
  :ensure t
  :init
  (setq savehist-file "~/.emacs-history")
  (setq savehist-length 1000)
  :config
  (savehist-load))

;;=======================================
;;                LaTeX
;;=======================================
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-PDF-mode t)
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.
  )

(use-package reftex
  :defer t
  :ensure t
  :init
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;;=======================================
;;                 Coq
;;=======================================
;; Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")
;; Load company-coq when opening Coq files
(use-package company-coq
  :init
  (add-hook 'coq-mode-hook #'company-coq-mode))

;;=======================================
;;               Mode Caml
;;=======================================
;; Setup environment variables using opam
(dolist
   (var (car (read-from-string
           (shell-command-to-string "opam config env --sexp"))))
 (setenv (car var) (cadr var)))
(setq exec-path (split-string (getenv "PATH") path-separator))
(push (concat (getenv "OCAML_TOPLEVEL_PATH")
          "/../../share/emacs/site-lisp") load-path)

(use-package utop
  :defer t
  :ensure t
  :init
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package tuareg
  :defer t
  :ensure t
  ;; :config
  ;; (setq tuareg-prettify-symbol-mode t)
  )

(use-package ocp-indent
  :defer t
  :ensure t)

(use-package merlin
  :defer t
  :ensure t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'tuareg-mode-hook 'auto-complete-mode t)
  (setq merlin-use-auto-complete-mode 'easy)
  (setq merlin-ac-setup 'easy)
  (setq merlin-command 'opam))

(setq auto-mode-alist (cons '("\\.ml[iylp]?" . tuareg-mode) auto-mode-alist))
(use-package caml-font :defer t)

;; Choose modes for related config. files
(setq auto-mode-alist
      (append '(("_oasis\\'" . conf-mode)
		("_tags\\'" . conf-mode)
		("_log\\'" . conf-mode))
              auto-mode-alist))

;;=======================================
;;              Mode texte
;;=======================================
;(setq default-justification 'left))
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (setq default-justification 'left)))

;;=======================================
;;              MINI-BUFFER
;;=======================================
;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; highlight trailing whitespaces in all modes
(setq-default show-trailing-whitespace t)

;;=======================================
;;              DOS - UNIX
;;=======================================
;; convert a buffer from DOS `^M' end of lines to Unix end of lines
(defun dos-to-unix ()
  "Cut all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))
;; convert a buffer from Unix end of lines to DOS `^M' end of lines
(defun unix-to-dos ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))))

;;=======================================
;;              ORG MODE
;;=======================================
(use-package org
  :defer t
  :ensure t
  :init
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "|" "DONE")
          (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
          (sequence "|" "CANCELED")))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time))

;;=======================================
;;             DIFF MODE
;;=======================================
(use-package diff-hl
  :defer t
  :ensure t
  :config
  (global-diff-hl-mode +1))

;;=======================================
;;             MAGIT
;;=======================================
(use-package magit
  :defer t
  :ensure t
  :bind
  ("C-x g" . magit-status))

;;=======================================
;;              HELM
;;=======================================
(use-package helm :ensure t)
(use-package helm-config
  :ensure helm
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ;; ("C-x C-f" . helm-find-files)
   ;; ("C-x 5 f" . helm-find-files)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :config
  (global-unset-key (kbd "C-x c"))
  (setq helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1))
