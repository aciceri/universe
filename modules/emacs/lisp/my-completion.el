;;; my-completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("<escape>" . abort-recursive-edit)
              ("M-." . vertico-insert)))

;; dmenu-style completion: vertico (and thus every completing-read) shows
;; up in a centered child frame instead of the bottom minibuffer. Consult
;; previews stay in the real window only; the mirrored thumbnail overlaid
;; on the candidate frame is just noise.
(use-package vertico-buffer-frame
  :after vertico
  :demand t
  :custom
  (vertico-buffer-frame-consult-preview nil)
  ;; Slightly glassy candidate frame; text stays fully opaque (pgtk's
  ;; alpha-background only fades the background pixels).
  (vertico-buffer-frame-parameters '((alpha-background . 90)))
  :config
  (vertico-buffer-frame-mode 1))

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

(provide 'my-completion)
;;; my-completion.el ends here
