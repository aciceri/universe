;;; my-ui.el --- Theme and visual niceties -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package catppuccin-theme
  :config
  (load-theme 'catppuccin :no-confirm))

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

(provide 'my-ui)
;;; my-ui.el ends here
