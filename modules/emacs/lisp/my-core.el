;;; my-core.el --- Base defaults, PATH, server, direnv -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :init
  ;; Emacs creates backup dirs on demand but NOT auto-save/lock-file targets;
  ;; a missing ~/.emacs-saves/ makes every auto-save error out.
  (make-directory "~/.emacs-saves/" t)
  ;; No titlebar: OmniWM tiles the frames, so the native decoration is
  ;; wasted vertical space. Applies to every frame, emacsclient ones too.
  (add-to-list 'default-frame-alist '(undecorated . t))
  :custom
  ;; Performance
  (gc-cons-threshold 100000000)
  (gc-cons-percentage 0.1)
  (native-comp-async-report-warnings-errors nil)

  ;; UI
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (use-dialog-box nil)
  (scroll-margin 3)
  (scroll-conservatively 101)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start t)
  (show-paren-when-point-inside-paren t)

  ;; Theme
  (catppuccin-flavor 'mocha)

  ;; Editing
  (indent-tabs-mode nil)
  (tab-width 2)
  (standard-indent 2)
  (treesit-font-lock-level 4)

  ;; Backups & Auto-save
  (backup-directory-alist '(("." . "~/.emacs-saves/")))
  (version-control t)
  (kept-new-versions 5)
  (kept-old-versions 2)
  (delete-old-versions t)
  (lock-file-name-transforms '((".*" "~/.emacs-saves/" t)))
  (auto-save-file-name-transforms '((".*" "~/.emacs-saves/" t)))
  (auto-save-timeout 30)
  (auto-save-interval 300)
  (auto-revert-verbose nil)

  ;; Session
  (recentf-max-saved-items 50)
  (confirm-kill-emacs 'y-or-n-p)
  (isearch-wrap-pause 'no)
  (search-highlight t)
  (ring-bell-function 'ignore)

  :init
  ;; UI modes
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode t)
  (column-number-mode t)
  (show-paren-mode t)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)

  ;; Theming is owned by catppuccin-theme (stylix's emacs target is
  ;; disabled: its base16 theme is less polished and used to shadow
  ;; catppuccin), so set the font ourselves. Same family the terminal
  ;; uses; 13pt on Linux, bigger on the macOS Retina display.
  (set-face-attribute 'default nil
                      :family "Iosevka Comfy"
                      :height (if (eq system-type 'darwin) 170 130))

  ;; Editing
  (electric-pair-mode t)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (global-visual-line-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; File management
  (global-auto-revert-mode t)
  (recentf-mode t)
  (save-place-mode t)

  ;; Nerd icons: pin the private-use-area ranges to the dedicated symbols
  ;; font (same glyph source Ghostty's patched fonts use). Without this,
  ;; fallback lands on whichever patched Nerd font sorts first, with
  ;; mismatched metrics.
  (when (member "Symbols Nerd Font Mono" (font-family-list))
    (dolist (range '((#xe000 . #xf8ff) (#xf0000 . #xfffff)))
      (set-fontset-font t range "Symbols Nerd Font Mono"))))

;; macOS GUI apps (and the Mac port) start with a minimal PATH; expose the
;; nix-profile tools (LSP servers, direnv, …) to Emacs and eglot.
(let ((nix-bin (expand-file-name "~/.nix-profile/bin")))
  (when (file-directory-p nix-bin)
    (add-to-list 'exec-path nix-bin)
    (setenv "PATH" (concat nix-bin path-separator (getenv "PATH")))))

;; Start the server unless one is already running (e.g. when launched as a
;; daemon, or when the launchd agent already spawned a GUI instance whose
;; server owns the socket).
(use-package server
  :config
  (unless (or (daemonp) (server-running-p))
    (server-start)))

(use-package envrc
  :init
  (envrc-global-mode))

(provide 'my-core)
;;; my-core.el ends here
