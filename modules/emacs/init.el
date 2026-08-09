;;; init.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin loader: the configuration lives in lisp/, one file per concern.
;; Both this file and lisp/ are out-of-store symlinks into the flake
;; checkout, so edits apply on the next daemon restart without a rebuild.

;;; Code:

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'my-core)       ; defaults, fonts, backups, PATH, server, direnv
(require 'my-ui)         ; theme, indent bars, which-key
(require 'my-completion) ; vertico, consult, orderless, marginalia, corfu
(require 'my-coding)     ; diagnostics, eglot, tree-sitter modes
(require 'my-hel)        ; Helix editing model + leader
(require 'my-terminal)   ; ghostel
(require 'my-niri)       ; niri IPC, frame mapping, window picker
(require 'my-agents)     ; agent-shell + Emacs MCP server

;;; init.el ends here
