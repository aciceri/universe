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

;; A TUI running inside a ghostel buffer — lazygit, mostly — asks to edit a
;; file by running the `emacs-edit-here' wrapper from
;; modules/emacs/emacs.nix, which calls this. Replacing the buffer in the
;; selected window means the file takes over the very window the terminal was
;; in, instead of emacsclient popping a separate frame somewhere else.
;;
;; emacsclient's own frameless mode is not usable for this: it prints
;; "Waiting for Emacs..." and never visits the file at all.
(defun my/edit-here (file &optional line)
  "Visit FILE in the selected window, at LINE when given."
  (switch-to-buffer (find-file-noselect file))
  (when (and (numberp line) (> line 1))
    (goto-char (point-min))
    (forward-line (1- line)))
  (buffer-name))

(provide 'my-terminal)
;;; my-terminal.el ends here
