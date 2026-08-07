;;; init.el -*- lexical-binding: t; -*-

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

  ;; Stylix injects the font (13pt, shared with the terminal) via the
  ;; generated default.el, which loads AFTER init.el. On macOS 13pt is too
  ;; small for the Retina display, so bump Emacs once startup is done (it
  ;; then wins over stylix); on Linux the stylix size is right — keep it.
  (when (eq system-type 'darwin)
    (add-hook 'after-init-hook
              (lambda () (set-face-attribute 'default nil :height 170))))

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

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("<escape>" . abort-recursive-edit)
              ("M-." . vertico-insert)))

(use-package eat)

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
  ;; Render diagnostics at the end of the offending line (like Helix's
  ;; end-of-line-diagnostics) instead of on a virtual line above it, which
  ;; made line spacing jump when the cursor idled on a diagnosed line.
  (flyover-show-at-eol t)
  (flyover-show-virtual-line nil)
  (flyover-wrap-messages nil)
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

(use-package which-key
  :init
  (which-key-mode))

(use-package hel
  :demand t
  :custom
  ;; Match Helix: block cursor in normal mode, bar while inserting
  ;; (hel ships them the other way around).
  (hel-normal-state-cursor-type 'box)
  (hel-insert-state-cursor-type 'bar)
  :config
  (hel-mode)
  ;; hel-leader turns SPC into a gateway to the "C-c" prefix ("SPC x" -> C-x),
  ;; so the former "SPC ..." leader bindings now live under "C-c ...".

  ;; Window management: SPC w {v,s,q,o,h,j,k,l}
  (keymap-global-set "C-c w v" #'split-window-right)
  (keymap-global-set "C-c w s" #'split-window-below)
  (keymap-global-set "C-c w q" #'delete-window)
  (keymap-global-set "C-c w o" #'delete-other-windows)
  (keymap-global-set "C-c w h" #'windmove-left)
  (keymap-global-set "C-c w j" #'windmove-down)
  (keymap-global-set "C-c w k" #'windmove-up)
  (keymap-global-set "C-c w l" #'windmove-right)

  ;; consult integration: SPC f / SPC b / SPC / / SPC d
  (keymap-global-set "C-c f" #'consult-project-extra-find)
  (keymap-global-set "C-c b" #'consult-project-buffer)
  (keymap-global-set "C-c /" #'consult-ripgrep)
  (keymap-global-set "C-c d" #'consult-flymake))

;; SPC leader: bridges SPC to the C-c/C-x prefixes, with which-key previews.
(use-package hel-leader
  :after hel)

;; Org-mode editing model.
(use-package hel-org
  :after (hel org))

;; libghostty terminal. The grammar/module is shipped by nix, so never
;; auto-download it at runtime.
(use-package ghostel
  :bind (("C-c o t" . ghostel))
  :config
  ;; show-paren flags the nushell prompt arrow 〉 as an unmatched paren,
  ;; painting it with show-paren-mismatch until the cursor moves.
  (add-hook 'ghostel-mode-hook (lambda () (show-paren-local-mode -1))))

;; Helix editing model inside ghostel buffers; without this hel-local-mode
;; has no terminal state and swallows all self-inserting keys.
(use-package hel-ghostel
  :after (hel ghostel)
  :demand t)

(use-package eglot
  :after hel
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  ;; Keep code-action availability in eldoc only: the default margin
  ;; indicator (an emoji from a fallback font) is taller than Iosevka and
  ;; visibly grows the cursor line after eldoc-idle-delay.
  (eglot-code-action-indications '(eldoc-hint))
  :config
  ;; Register language servers
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nixd")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs '((js-ts-mode typescript-ts-mode tsx-ts-mode) . ("vtsls" "--stdio")))
  (add-to-list 'eglot-server-programs '(terraform-ts-mode . ("terraform-ls" "serve")))

  ;; LSP keymap under SPC l (the C-c l prefix, reached via hel-leader)
  (keymap-global-set "C-c l s" #'eglot-shutdown)
  (keymap-global-set "C-c l r" #'eglot-rename)
  (keymap-global-set "C-c l a" #'eglot-code-actions)
  (keymap-global-set "C-c l f" #'eglot-format)
  (keymap-global-set "C-c l h" #'eldoc-doc-buffer)
  (keymap-global-set "C-c l d" #'xref-find-definitions)
  (keymap-global-set "C-c l t" #'eglot-find-typeDefinition)
  (keymap-global-set "C-c l i" #'eglot-find-implementation)

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
  :after (consult eglot)
  :config
  (keymap-global-set "C-c l g" #'consult-eglot-symbols))

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

;; Coding agents via ACP. Layout stolen from helheim-emacs: the first
;; shell opens in another window, every subsequent agent buffer reuses
;; that window. Default agent: Oh My Pi (`omp acp` speaks ACP natively).
(use-package agent-shell
  :custom
  (agent-shell-pi-acp-command '("omp" "acp"))
  (agent-shell-preferred-agent-config 'pi)
  (agent-shell-display-action nil)
  ;; Slim UI: collapse tool-call runs into a single expandable header line
  ;; (TAB expands), compact count-style header labels, and no welcome
  ;; banner / graphical SVG header / busy animation.
  (agent-shell-activity-group-expand-by-default nil)
  (agent-shell-show-welcome-message nil)
  (agent-shell-header-style 'text)
  (agent-shell-show-busy-indicator nil)
  :bind (("C-c a RET" . agent-shell)
         ("C-c a n" . agent-shell-new-shell)
         ("C-c a w" . agent-shell-new-worktree-shell)
         ("C-c a s" . agent-shell-send-dwim))
  :config
  (setq agent-shell-activity-group-header-label-function
        #'agent-shell-activity-group-count-label)
  (add-to-list 'display-buffer-alist
               '((or (major-mode . agent-shell-mode)
                     (major-mode . agent-shell-viewport-view-mode)
                     (major-mode . agent-shell-viewport-edit-mode))
                 (display-buffer-reuse-mode-window
                  display-buffer-pop-up-window)
                 (mode . (agent-shell-mode
                          agent-shell-viewport-view-mode
                          agent-shell-viewport-edit-mode)))))

(use-package hel-agent-shell
  :after (hel agent-shell))

;; MCP server exposing this Emacs session to LLM agents (buffers, elisp,
;; diagnostics, org tools). Connect with e.g.:
;;   claude mcp add emacs -- socat - UNIX-CONNECT:<socket printed by M-x mcp-server-status>
(use-package mcp-server
  :demand t
  :custom
  (mcp-server-security-prompt-for-permissions t)
  :config
  (with-eval-after-load 'org
    (setopt mcp-server-emacs-tools-org-allowed-roots (list org-directory)
            mcp-server-emacs-tools-org-auto-save t))
  ;; Don't ask about killing the server process when quitting Emacs.
  (defun my/mcp-server-no-query-on-exit (&rest _)
    (dolist (proc (list (bound-and-true-p mcp-server-transport-unix--server-process)
                        (bound-and-true-p mcp-server-transport-tcp--server-process)))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
  (advice-add 'mcp-server-transport-unix--start :after #'my/mcp-server-no-query-on-exit)
  (advice-add 'mcp-server-transport-tcp--start :after #'my/mcp-server-no-query-on-exit)
  (mcp-server-start))

;;; init.el ends here
