;;; my-coding.el --- Diagnostics, LSP, language modes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(provide 'my-coding)
;;; my-coding.el ends here
