;;; my-hel.el --- Helix editing model -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

;; Hel bindings for third-party packages; the agent-shell integration
;; moved here from the archived hel-agent-shell. Init only registers
;; with-eval-after-load forms, so it is cheap to call eagerly.
(use-package hel-collection
  :after hel
  :demand t
  :config
  (hel-collection-init '(agent-shell)))

(provide 'my-hel)
;;; my-hel.el ends here
