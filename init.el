;;=======================================
;; Startup process adapted from:
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;=======================================
(setq package-enable-at-startup nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;;=======================================
;;               EMACS PATH
;;=======================================
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(let ((default-directory  "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'local nil t)
(require 'private nil t)
(setq auth-sources '((:source "~/.authinfo.gpg")))

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

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;;=======================================
;;              GLOBAL FLAGS
;;=======================================
;; Start server only for main graphical Emacs
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package diminish)

(setq custom-file (concat user-emacs-directory "/custom.el"))
(load-file custom-file)

(add-to-list 'default-frame-alist '(width . 81))

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)

;; Backup file settings
(setq backup-by-copying t      ; don't clobber symlinks
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
(global-hl-line-mode t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode t)
(column-number-mode t)
(setq initial-scratch-message ""
      inhibit-startup-message t
      inhibit-startup-echo-area-message "")

(if (member "Hack" (font-family-list))
    (set-frame-font "Hack-10" nil t)
  (set-frame-font "DejaVu Sans Mono-10" nil t))

(use-package all-the-icons)

(use-package doom-themes
  :config
  (doom-themes-org-config)
  ;; Theme cycling
  ;; taken from:
  ;; https://github.com/habamax/.emacs.d/blob/master/lisp/haba-appearance.el
  (defvar *my-theme-light* 'doom-one-light)
  (defvar *my-theme-dark* 'doom-one)
  (defvar *my-current-theme* *my-theme-dark*)
  (load-theme *my-theme-dark*)
  ;; disable other themes before loading new one
  (defadvice load-theme (before theme-dont-propagate activate)
    "Disable theme before loading new one."
    (mapc #'disable-theme custom-enabled-themes))
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
  )

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-file-name-style 'buffer-name
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-env-python-executable "python3"
        )
  )

(use-package neotree
  :bind ([f3] . neotree-toggle)
  :config
  (doom-themes-neotree-config)
  )

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
(define-key global-map [f4]  'goto-char)
(define-key global-map [f5]  'goto-line)
(define-key global-map [f6]  'compile)
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
;;             UNDOING
;;=======================================
(use-package undo-tree
  :diminish undo-tree-mode
  :bind* (("C-x u" . undo-tree-undo)
          ("C-_" . undo-tree-undo)
          ("M-m r" . undo-tree-redo)
          ("M-m U" . undo-tree-visualize))
  :config
  (global-undo-tree-mode 1))

;;=======================================
;;           AUTO-COMPLETION
;;=======================================
(use-package auto-complete
  :defer t
  :init (setq ac-auto-start nil)
  :bind (:map ac-mode-map ("<C-tab>" . auto-complete)))

;;=======================================
;;             MULTI-CURSOR
;;=======================================
(use-package multiple-cursors
  :defer t
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
  :init
  (setq savehist-file "~/.emacs-history")
  (setq savehist-length 1000)
  :config
  (savehist-mode +1))

;;=======================================
;;                LaTeX
;;=======================================
(use-package latex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :bind (:map LaTeX-mode-map ("C-c l" . TeX-error-overview))
  :init
  (use-package company-auctex :init (company-auctex-init))
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-PDF-mode)
              (turn-on-reftex)
              (TeX-debug-bad-boxes)
              (TeX-debug-warnings)
              (TeX-source-correlate-mode)
              )
            )
  :config
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.
  (setq-default TeX-clean-confirm nil)
  (setq-default TeX-master nil) ; in newer versions: dwim
  (add-hook 'TeX-language-fr-hook
            (lambda () (ispell-change-dictionary "francais")))
  (add-hook 'TeX-language-en-hook
            (lambda () (ispell-change-dictionary "english")))
  )

(use-package reftex
  :defer t
  :init
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(setq ispell-program-name "aspell")

(use-package writegood-mode
  :bind
  ("C-c g"     . writegood-mode)
  ("C-c C-g g" . writegood-grade-level)
  ("C-c C-g e" . writegood-reading-ease))

;;=======================================
;;                 PDF
;;=======================================
(use-package pdf-view
  :ensure pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (setq pdf-annot-activate-created-annotations t)
  )

;;=======================================
;;                 Coq
;;=======================================
(use-package proof-general
  :mode ("\\.v\\'" . coq-mode)
  :init
  (setq proof-splash-enable nil
        proof-sticky-errors t)
  )

(use-package company-coq
  :commands (company-coq-mode)
  :hook (coq-mode . company-coq-mode)
  :config
  (setq company-coq-disabled-features '(hello))
  (setq
   coq-symbols-list
   (lambda ()
     (setq-local prettify-symbols-alist
                 '(("~" . ?¬) ("empty" . ?Ø) ("*" . ?×) ("\\in" . ?\u220A)
                   ("~exists" . ?\u2204)
                   ("Qed." . ?■) ("Defined." . ?□)
                   ("==>*" . (?\u27F9 (Br . Bl) ?*))
                   ("=?" . ?\u225F) ("<=?" . (?\u2264 (Br . Bl) ??))
                   ("[|" . ?\u27E6) ("|]" . ?\u27E7) ("\\|" . ?\u21D3)
                   ("|]\\|" . (?\u27E7 (Br . Bl) ?\u21D3))
                   ("\\(" . ?\u27E8) ("\\)" . ?\u27E9)
                   ("\\:" . ?\u2236) ("|=" . ?\u22A7)
                   ("Gamma'" . (?Γ (Br . Bl) ?'))
                   ("Gamma''" . (?Γ (Br . Bl) ?' (Br . Bl) ?'))
                   ("Gamma0" . (?Γ (Br . Bl) ?0))
                   ("Gamma1" . (?Γ (Br . Bl) ?1))
                   ("Gamma2" . (?Γ (Br . Bl) ?2))
                   ("sigma'" . (?σ (Br . Bl) ?'))
                   ("sigma''" . (?σ (Br . Bl) ?' (Br . Bl) ?'))
                   ("sigma0" . (?σ (Br . Bl) ?0))
                   ("sigma1" . (?σ (Br . Bl) ?1))
                   ("sigma2" . (?σ (Br . Bl) ?2))
                   ;; same as other capital letters -> confusing
                   ;; ("Alpha" . ?Α) ("Beta" . ?Β) ("Epsilon" . ?Ε) ("Zeta" . ?Ζ)
                   ;; ("Eta" . ?Η) ("Iota" . ?Ι) ("Kappa" . ?Κ) ("Mu" . ?Μ)
                   ;; ("Nu" . ?Ν) ("Omicron" . ?Ο) ("Rho" . ?Ρ) ("Tau" . ?Τ)
                   ;; ("Upsilon" . ?Υ) ("Chi" . ?Χ)
                   ;; OK
                   ("Gamma" . ?Γ) ("Delta" . ?Δ) ("Theta" . ?Θ) ("Lambda" . ?Λ)
                   ("Xi" . ?Ξ) ("Pi" . ?Π) ("Sigma" . ?Σ) ("Phi" . ?Φ)
                   ("Psi" . ?Ψ) ("Omega" . ?Ω)
                   ("alpha" . ?α) ("beta" . ?β) ("gamma" . ?γ)
                   ("delta" . ?δ) ("epsilon" . ?ε) ("zeta" . ?ζ)
                   ("eta" . ?η) ("theta" . ?θ) ("iota" . ?ι)
                   ("kappa" . ?κ) ("mu" . ?μ)
                   ("nu" . ?ν) ("xi" . ?ξ) ("omicron" . ?ο)
                   ("pi" . ?π) ("rho" . ?ρ) ("sigma" . ?σ)
                   ("tau" . ?τ) ("upsilon" . ?υ) ("phi" . ?φ)
                   ("chi" . ?χ) ("psi" . ?ψ)
                   ;; also confusing?
                   ("lambda" . ?λ) ("omega" . ?ω)
                   ))))
  (add-hook 'coq-mode-hook coq-symbols-list)
  (add-hook 'coq-goals-mode-hook coq-symbols-list)
  )

;;=======================================
;;               Mode Caml
;;=======================================
(use-package utop
  :defer t
  :hook (tuareg-mode . utop-minor-mode)
  :init
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package tuareg-mode
  :defer t
  :ensure tuareg
  :mode "\\.ml[iylp]?"
  :mode
  (("_oasis\\'" . conf-mode)
   ("_tags\\'" . conf-mode)
   ("_log\\'" . conf-mode))
  :init
  ;; Setup environment variables using opam
  (dolist
      (var (car (read-from-string
                 (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (setq exec-path (split-string (getenv "PATH") path-separator))
  (push (concat (getenv "OCAML_TOPLEVEL_PATH")
                "/../../share/emacs/site-lisp") load-path)
  ;; :config
  ;; (setq tuareg-prettify-symbol-mode t)
  )

(use-package merlin-mode
  :defer t
  :ensure merlin
  :hook tuareg-mode
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend))
  (setq merlin-command 'opam))

;;=======================================
;;              Mode Python
;;=======================================
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-shell-interpreter "python3"))

;;=======================================
;;              Mode texte
;;=======================================
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (setq default-justification 'left)))

;;=======================================
;;              MINI-BUFFER
;;=======================================
;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;;=======================================
;;      I HATE TRAILING WHITESPACE
;;=======================================
;; highlight trailing whitespaces in all modes
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;=======================================
;;           MOVING AROUND
;;=======================================
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

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
;;              CSV
;;=======================================
(use-package csv-mode
  :defer t
  :mode "\\.[Cc][Ss][Vv]\\'"
  )

;;=======================================
;;              ORG MODE
;;=======================================
(use-package org
  :defer t
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :init
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "|" "DONE")
          (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
          (sequence "|" "CANCELED")))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t
        org-pretty-entities t)
  (setq org-agenda-files '("~/Cloud/Org")
        org-default-notes-file "~/Cloud/Org/notes.org")
  )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (ocaml . t)
   (python . t)
   (R . t)))
(setq org-babel-python-command "python3")
(setq org-confirm-babel-evaluate nil)

;;=======================================
;;             MAIL
;;=======================================
(use-package wanderlust
  :commands (wl wl-other-frame)
  :bind ("C-c m" . wl-other-frame)
  :init
  (setq wl-summary-line-format "%T%P %D/%M (%W) %h:%m %t%[%25(%c %f%) %] %s"
        wl-summary-width 150)
  (setq wl-message-ignored-field-list '("^.*:")
        wl-message-visible-field-list
        '("^\\(To\\|Cc\\):"
          "^Subject:"
          "^From:"
          "^Organization:"
          "^\\(Posted\\|Date\\):"
          )
        wl-message-sort-field-list
        '("^From" "^Organization:" "^Subject" "^Date" "^To" "^Cc")
        )
  (setq wl-forward-subject-prefix "Fwd: " )    ;; use "Fwd: " not "Forward: "
  ;; from a WL-mailinglist post by David Bremner
  ;; Invert behaviour of with and without argument replies.
  ;; just the author
  (setq wl-draft-reply-without-argument-list
        '(("Reply-To" ("Reply-To") nil nil)
          ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
          ("From" ("From") nil nil)))
  ;; bombard the world
  (setq wl-draft-reply-with-argument-list
        '(("Followup-To" nil nil ("Followup-To"))
          ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
          ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
          ("From" ("From") ("To" "Cc") ("Newsgroups"))))
  (setq wl-use-folder-petname '(modeline ask-folder read-folder))
  (setq wl-summary-showto-folder-regexp ".*Sent.*")
  (setq mime-view-buttons-visible nil
        mime-view-mailcap-files '("~/.emacs.d/mailcap")
        mime-play-find-every-situations nil
        mime-play-delete-file-immediately nil)
  (add-hook 'mime-view-mode-hook
            #'(lambda () (setq show-trailing-whitespace nil)))
  )

;;=======================================
;;             MAGIT
;;=======================================
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;;=======================================
;;              HELM
;;=======================================
; (use-package helm))
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
  (setq helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1))

;;=======================================
;;            COMPANY
;;=======================================
(use-package company
  :hook (after-init . global-company-mode)
  :bind (("C-<tab>" . company-complete)
         :map company-active-map ("<tab>" . company-complete-common-or-cycle))
  :config
  (setq company-idle-delay nil ;; on-demand completion
        company-minimum-prefix-length 0
        company-show-numbers t
        company-selection-wrap-around t
        )
  )
