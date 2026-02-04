;;; init.el -*- lexical-binding: t; -*-

;;; Code:

(use-package emacs
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
  (save-place-mode t))

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("M-." . vertico-insert)))

(use-package consult
  :custom
  (consult-preview-key '(:debounce 0.2 any))
  :config
  (consult-customize
   consult-buffer :preview-key '(:debounce 0.2 any)
   consult-find :preview-key '(:debounce 0.2 any)
   consult-line :preview-key '(:debounce 0.2 any))
  :bind (;; C-c bindings for consult commands
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings for common actions
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ;; M-g bindings for goto commands
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings for search commands
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)))

(use-package consult-project-extra
  :after consult
  :config
  (plist-put consult-project-extra--source-file :state #'consult--file-state)
  (consult-customize consult-project-extra-find :preview-key '(:debounce 0.2 any)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
              ("M-d" . corfu-info-documentation)
              ("M-l" . corfu-info-location)))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-idle-delay 0.3)
  :config
  (global-eldoc-mode))

(use-package flymake
  :config
  (setq flymake-mode-line-lighter "Fly")
  :hook (eglot-managed-mode . flymake-mode))

(use-package flyover
  :hook (flymake-mode . flyover-mode)
  :custom
  (flyover-line-position-offset 0))

(use-package indent-bars
  :hook ((prog-mode . indent-bars-mode))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-scope-emphasis t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-wrap nil)
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-highlight-current-depth '(:blend 0.5)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package multiple-cursors)

(use-package helix
  :demand t
  :after multiple-cursors
  :config
  (helix-multiple-cursors-setup)
  (helix-mode)

  ;; Create window management keymap under SPC w
  (defvar helix-window-map (make-sparse-keymap) "Keymap for window commands")
  (define-key helix-space-map "w" helix-window-map)

  ;; Window management keybindings
  (define-key helix-window-map "v" #'split-window-right)
  (define-key helix-window-map "s" #'split-window-below)
  (define-key helix-window-map "q" #'delete-window)
  (define-key helix-window-map "o" #'delete-other-windows)
  (define-key helix-window-map "h" #'windmove-left)
  (define-key helix-window-map "j" #'windmove-down)
  (define-key helix-window-map "k" #'windmove-up)
  (define-key helix-window-map "l" #'windmove-right)

  ;; Override SPC f, SPC b, and SPC / for consult integration
  (define-key helix-space-map "f" #'consult-project-extra-find)
  (define-key helix-space-map "b" #'consult-project-buffer)
  (define-key helix-space-map "/" #'consult-ripgrep)

  ;; Diagnostics - SPC d for diagnostics (similar to helix's space+d)
  (define-key helix-space-map "d" #'consult-flymake))

(use-package eglot
  :after helix
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  :config
  ;; Register language servers
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nixd")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs '((js-ts-mode typescript-ts-mode tsx-ts-mode) . ("vtsls" "--stdio")))
  (add-to-list 'eglot-server-programs '(terraform-ts-mode . ("terraform-ls" "serve")))

  ;; Create LSP keymap under SPC l (following helix pattern)
  (defvar helix-lsp-map (make-sparse-keymap) "Keymap for LSP commands")
  (define-key helix-space-map "l" helix-lsp-map)

  ;; LSP keybindings
  (define-key helix-lsp-map "s" #'eglot-shutdown)
  (define-key helix-lsp-map "r" #'eglot-rename)
  (define-key helix-lsp-map "a" #'eglot-code-actions)
  (define-key helix-lsp-map "f" #'eglot-format)
  (define-key helix-lsp-map "h" #'eldoc-doc-buffer)
  (define-key helix-lsp-map "d" #'xref-find-definitions)
  (define-key helix-lsp-map "t" #'eglot-find-typeDefinition)
  (define-key helix-lsp-map "i" #'eglot-find-implementation)

  :hook ((nix-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (terraform-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("M-RET" . eglot-code-actions)))

(use-package consult-eglot
  :after (consult eglot helix)
  :config
  (define-key helix-lsp-map "g" #'consult-eglot-symbols))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package python-ts-mode
  :mode "\\.py\\'")

(use-package rust-ts-mode
  :mode "\\.rs\\'")

(use-package haskell-ts-mode
  :mode "\\.hs\\'")

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

(use-package js-ts-mode
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2))

(use-package html-ts-mode
  :mode "\\.html\\'")

(use-package css-ts-mode
  :mode "\\.css\\'")

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'")

(use-package json-ts-mode
  :mode "\\.json\\'")

(use-package toml-ts-mode
  :mode "\\.toml\\'")

(use-package terraform-ts-mode
  :mode "\\.tf\\'")

(use-package agent-shell
  :config
  (setopt agent-shell-provider 'anthropic)
  :bind (("C-c a a" . agent-shell-chat)
         ("C-c a r" . agent-shell-region)
         ("C-c a b" . agent-shell-buffer)
         ("C-c a f" . agent-shell-file)
         ("C-c a s" . agent-shell-summarize)))

;;; init.el ends here
