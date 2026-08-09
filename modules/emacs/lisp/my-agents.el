;;; my-agents.el --- Coding agents and the Emacs MCP server -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
  ;; DWIM context carried into a new shell: keep deliberate sources only.
  ;; The default also includes `line', which silently copies the current
  ;; line into the prompt when starting a shell from any buffer.
  (agent-shell-context-sources '(files region error))
  :bind (("C-c a RET" . agent-shell)
         ("C-c a n" . agent-shell-new-shell)
         ("C-c a w" . agent-shell-new-worktree-shell)
         ("C-c a s" . agent-shell-send-dwim)
         :map agent-shell-mode-map
         ;; Queue a prompt while the agent is busy (auto-submits when idle);
         ;; sends immediately when idle. @ completes files, / commands.
         ("C-c RET" . agent-shell-queue-request)
         ;; Newline in the prompt: S-RET is the shell-maker default, M-RET
         ;; matches the Alt+Enter habit from terminal TUIs.
         ("M-RET" . newline))
  :config
  (setq agent-shell-activity-group-header-label-function
        #'agent-shell-activity-group-count-label)
  ;; YOLO mode: auto-approve every tool permission request.
  (setq agent-shell-permission-responder-function
        #'agent-shell-permission-allow-always)
  ;; ACP clients pass MCP servers at session/new; omp acp mounts ONLY these
  ;; (its own user mcp.json is listed by /mcp but not mounted), and it does
  ;; NOT inject stored OAuth creds into them (v17.2.8: the ACP session
  ;; factory forces enableMCP:false; see oh-my-pi#1525/#1234). Workaround:
  ;; read the fresh access token from omp's own auth storage (agent.db,
  ;; sqlite) and pass it as an Authorization header. Tokens rotate (slack
  ;; ~1h, grain ~2h), so the list is rebuilt before every session start.
  (defun my/omp-mcp-token (url)
    "Return the active omp MCP OAuth access token for URL, or nil."
    (when-let* ((db-file (expand-file-name "~/.omp/agent/agent.db"))
                ((file-readable-p db-file))
                (db (sqlite-open db-file))
                (row (car (sqlite-select
                           db
                           "SELECT data FROM auth_credentials
                            WHERE provider = ? AND disabled_cause IS NULL
                            ORDER BY updated_at DESC LIMIT 1"
                           (list (concat "mcp_oauth:profile:default:" url))))))
      (prog1 (gethash "access" (json-parse-string (car row)))
        (sqlite-close db))))
  (defun my/agent-shell-refresh-mcp-servers (&rest _)
    "Rebuild `agent-shell-mcp-servers' with fresh tokens from omp's storage."
    (setq agent-shell-mcp-servers
          (append
           (delq nil
                 (mapcar
                  (lambda (spec)
                    (pcase-let ((`(,name . ,url) spec))
                      (let ((token (my/omp-mcp-token url)))
                        `((name . ,name) (type . "http") (url . ,url)
                          ,@(when token
                              `((headers . (((name . "Authorization")
                                             (value . ,(concat "Bearer " token)))))))))))
                  '(("grain" . "https://api.grain.com/_/mcp")
                    ("linear" . "https://mcp.linear.app/mcp")
                    ("slack" . "https://mcp.slack.com/mcp"))))
           `(((name . "gcal")
              (command . "nix")
              (args . ("shell" "nixpkgs#nodejs_22" "-c" "npx" "-y" "@cocal/google-calendar-mcp"))
              (env . (((name . "GOOGLE_OAUTH_CREDENTIALS")
                       (value . ,(expand-file-name "~/.config/gcp-oauth.keys.json")))))))
           (when-let* ((cmd (executable-find "emacs-mcp-stdio")))
             `(((name . "emacs") (command . ,cmd)))))))
  (advice-add 'agent-shell--start :before #'my/agent-shell-refresh-mcp-servers)
  (add-to-list 'display-buffer-alist
               '((or (major-mode . agent-shell-mode)
                     (major-mode . agent-shell-viewport-view-mode)
                     (major-mode . agent-shell-viewport-edit-mode))
                 (display-buffer-reuse-mode-window
                  display-buffer-pop-up-window)
                 (mode . (agent-shell-mode
                          agent-shell-viewport-view-mode
                          agent-shell-viewport-edit-mode)))))

;; MCP server exposing this Emacs session to LLM agents (buffers, elisp,
;; diagnostics, org tools). omp picks it up automatically when launched
;; from inside Emacs: ~/.omp/agent/mcp.json points at emacs-mcp-stdio,
;; which dials the socket exported below as EMACS_MCP_SOCKET. Other
;; clients can connect with e.g.:
;;   claude mcp add emacs -- emacs-mcp-stdio
(use-package mcp-server
  :demand t
  :custom
  ;; Free rein for MCP clients: nothing is treated as dangerous or
  ;; sensitive, so tool calls are always allowed without prompting.
  (mcp-server-security-prompt-for-permissions nil)
  (mcp-server-security-dangerous-functions nil)
  (mcp-server-security-sensitive-file-patterns nil)
  (mcp-server-security-sensitive-buffer-patterns nil)
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
  (mcp-server-start)
  ;; Expose the socket to every Emacs subprocess so agent-shell's omp
  ;; (and any shell spawned in here) can reach this session's MCP server.
  (when (bound-and-true-p mcp-server-transport-unix--socket-path)
    (setenv "EMACS_MCP_SOCKET" mcp-server-transport-unix--socket-path)))

(provide 'my-agents)
;;; my-agents.el ends here
