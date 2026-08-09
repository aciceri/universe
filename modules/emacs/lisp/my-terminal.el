;;; my-terminal.el --- libghostty terminal -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(provide 'my-terminal)
;;; my-terminal.el ends here
