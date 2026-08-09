;;; my-niri.el --- niri compositor integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; niri IPC integration: live compositor state mirrored into Emacs plus a
;; frame<->niri-window mapping (zero-width chars appended to frame titles).
;; Only meaningful inside a niri session, hence the NIRI_SOCKET guard.
(use-package niri-rpc
  :if (getenv "NIRI_SOCKET")
  :demand t
  :config
  (niri-rpc-connect))

;; niri-frame relies on `frame-id', which lands in Emacs 31. Shim it with a
;; frame parameter + counter until then; the ids only need to be unique and
;; stable for a frame's lifetime, which this guarantees.
(unless (fboundp 'frame-id)
  (defvar my/frame-id-counter 0)
  (defun frame-id (&optional frame)
    "Compat shim for the Emacs 31 `frame-id'."
    (let ((frame (or frame (selected-frame))))
      (or (frame-parameter frame 'my/frame-id)
          (let ((id (setq my/frame-id-counter (1+ my/frame-id-counter))))
            (set-frame-parameter frame 'my/frame-id id)
            id)))))

(use-package niri-frame
  :if (getenv "NIRI_SOCKET")
  :after niri-rpc
  :demand t
  :config
  (niri-frame-enable))

;; Minibuffer window switcher over the live niri state. DMS spotlight stays
;; the global switcher; this one is for jumping without leaving Emacs.
(defun my/consult-niri-window ()
  "Pick a niri window with completion and focus it."
  (interactive)
  (unless (niri-rpc-connected-p)
    (niri-rpc-connect))
  (let* ((windows (niri-rpc-windows))
         (candidates
          (mapcar (lambda (win)
                    (let ((title (or (niri-rpc-window-title win) "")))
                      ;; Emacs frames carry an invisible zero-width frame-id
                      ;; suffix in their titles; don't leak it into candidates.
                      (when (fboundp 'niri-frame--strip-encoding)
                        (setq title (niri-frame--strip-encoding title)))
                      ;; Trailing id keeps same-titled windows distinct.
                      (cons (format "%s: %s  #%d"
                                    (niri-rpc-window-app-id win)
                                    title
                                    (niri-rpc-window-id win))
                            (niri-rpc-window-id win))))
                  windows)))
    (unless candidates
      (user-error "No niri windows"))
    (niri-rpc-focus-window
     (cdr (assoc (completing-read "niri window: " candidates nil t)
                 candidates)))))
(keymap-global-set "C-c o w" #'my/consult-niri-window)

(provide 'my-niri)
;;; my-niri.el ends here
