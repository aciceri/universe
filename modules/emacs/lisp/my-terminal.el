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

;; Workaround for https://github.com/dakra/ghostel/issues/673: once a
;; buffer has shown a kitty-graphics image, `ghostel--kitty-display-virtual'
;; rescans the WHOLE buffer for U+10EEEE placeholders on every redraw
;; (~30fps while output flows). Profiled 2026-09-04 on this daemon: ~82%
;; of CPU in that scan, starving the single Lisp thread — every buffer,
;; scratch included, lagged whenever a terminal was printing. Disable
;; kitty graphics entirely until upstream limits the scan to the viewport;
;; drop both lines to get inline images (yazi/timg previews) back.
(with-eval-after-load 'ghostel
  (setq ghostel-kitty-graphics-storage-limit 0)  ; new terminals: module ignores transmissions
  (advice-add 'ghostel--kitty-display-virtual :override #'ignore) ; live buffers: skip the scan

  ;; Redraw-cost tuning, profiled 2026-09-04 with several terminals
  ;; streaming agent output (the daemon is one Lisp thread; every ms a
  ;; redraw eats is a ms every other buffer's keystroke waits):
  ;;
  ;; - `window-text-pixel-size' in `ghostel--pixel-anchor' was 48% of
  ;;   daemon CPU (a simulated redisplay layout per anchored window per
  ;;   redraw). Its line-count fallback is exact while row heights are
  ;;   uniform, and they are: kitty images are disabled above. Internal
  ;;   variable, so re-check when bumping ghostel.
  (setq ghostel--pixel-anchor-supported-p nil)
  ;; - Cap redraws at 20fps instead of 30; adaptive-fps still gives
  ;;   interactive typing its immediate-echo fast path.
  (setq ghostel-timer-delay 0.05)
  ;; - Batch plain-URL linkification harder under sustained output
  ;;   (was 10% of CPU at the default 0.1s debounce).
  (setq ghostel-plain-link-detection-delay 0.5)

  ;; Terminal workloads (omp agents, builds, rg) run as *children of the
  ;; daemon* — same emacs.service cgroup — so no systemd CPUWeight can
  ;; separate them from the Lisp thread; inside one cgroup only nice
  ;; decides. Spawn terminal shells at nice 10: Emacs (nice 0) preempts
  ;; agent load whenever it has redraw/input work, and the shells get the
  ;; full machine the moment Emacs is idle. nix builds are already
  ;; SCHED_IDLE via nix.daemonCPUSchedPolicy in modules/nix.nix.
  (setq ghostel-shell (list "nice" "-n10" (or (getenv "SHELL") "/bin/sh"))))

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
