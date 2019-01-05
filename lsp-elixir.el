;;; lsp-elixir.el --- Elixir tooling integration into Emacs -*- lexical-binding: t; -*-

;; Copyright © 2018 Aldric Giacomoni
;;
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Maintainer: Aldric Giacomoni <trevoke@gmail.com>
;; URL: http://www.github.com/trevoke/lsp-elixir.el
;; Version: 0.1.0
;; Package-Requires: ((lsp-mode "20190104.2105") (emacs "24.4"))
;; Keywords: languages, elixir, elixirc, mix, hex, alchemist

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Install through MELPA, then add the following line to your config:
;; `(add-hook 'elixir-mode-hook #'lsp)`

;;; Code:

(require 'lsp-mode)

(defgroup lsp-elixir-server nil
  "Which Language Server Protocol lsp-elixir will use"
  :prefix "lsp-elixir-server-"
  :group 'lsp-elixir)

(defcustom lsp-elixir-server-extension nil
  "Help lsp-elixir decide if you are running on a *nix or Microsoft machine."
  :type '(choice (const :tag "Windows" bat)
                 (const :tag "*nix" sh))
  :group 'lsp-elixir-server)

(defconst lsp-elixir-server-root-path
  (concat (file-name-directory (or load-file-name buffer-file-name)) "elixir-ls/"))

(defvar lsp-elixir--project-settings nil
  "Where lsp-elixir keeps its project-level settings.")

(defvar lsp-elixir-project-root-path-cache nil
  "Variable which holds the cached project root path.")

(defconst lsp-elixir-project-hex-pkg-indicator ".hex"
  "File which indicates the root directory of an Elixir Hex package.")

(defconst lsp-elixir-project-mix-project-indicator "mix.exs"
  "File which indicates the root directory of an Elixir Mix project.")

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-elixir--lsp-server-path-for-current-project)
                  :major-modes '(elixir-mode)
                  :priority 1
                  :server-id 'elixir-ls))

;; '("~/src/projects/lsp-elixir.el/elixir-ls/erl19/language_server.sh")

;; TODO what to do with these hooks?
;; (add-hook 'lsp-elixir-mode-hook 'lsp-ui-mode)
;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

;; file/dir utilities

(defun lsp-elixir--root-dir ()
  "Private function to find the root directory for a given file."
  (or lsp-elixir-project-root-path-cache
      (setq-local lsp-elixir-project-root-path-cache
                  (lsp-elixir-project-root-or-default-dir))))

(defun lsp-elixir-project-root-or-default-dir (&optional dir)
  "Return the root directory of the project or nil if not in a project.

Optional argument DIR is the starting directory. If not provided, use directory for current buffer."
  (let* ((starting-directory (file-name-directory (or dir default-directory)))
         (root (lsp-elixir--find-next-possible-root starting-directory)))
    (if root
        (lsp-elixir--find-next-possible-root
         (file-name-directory (directory-file-name root)))
      starting-directory)))

(defun lsp-elixir--find-next-possible-root (dir)
  "Find the next directory closer to the root.
Returns nil if no new root is found.
Argument DIR is the directory from which to start traversing up the tree."
  (or (locate-dominating-file dir lsp-elixir-project-mix-project-indicator)
      (locate-dominating-file dir lsp-elixir-project-hex-pkg-indicator)))

;; server version utilities

(defun lsp-elixir--lsp-server-path-for-current-project ()
  "Private function to find the path to the relevant LSP server."
  `,(concat lsp-elixir-server-root-path
             "erl"
             (lsp-elixir--server-erlang-version (lsp-elixir--root-dir))
             "/"
             "language_server"
             "."
             (lsp-elixir--server-extension)))

(defun lsp-elixir--server-erlang-version (project-path)
  "Private function to find which Erlang version to use for the given project.
Argument PROJECT-PATH is the path to the target project."
  (let* ((project-settings-map (lsp-elixir--project-settings))
         (project-erlang-version (or (gethash project-path
                                              (gethash "lsp-elixir-projects" project-settings-map))
                                     (completing-read "Choose which version of Erlang the LSP server should use: "
                                                      '("19" "20" "21")
                                                      nil
                                                      t
                                                      ))))
    (puthash project-path project-erlang-version (gethash "lsp-elixir-projects" project-settings-map))
    (setq lsp-elixir--project-settings project-settings-map)
    (lsp-elixir--project-save-settings project-settings-map)
    project-erlang-version))

(defun lsp-elixir--server-extension ()
  "Private function to find the kind of executable to run on the host OS."
  (let ((extension (or lsp-elixir-server-extension
                       (completing-read "Choose the kind of executable that runs on this system: "
                                        '("sh" "bat")
                                        nil
                                        t
                                        ))))
    (setq lsp-elixir-server-extension extension)
    extension))

(defun lsp-elixir--project-settings ()
  "Private function to find the project settings (Elixir / Erlang version)."
  (or lsp-elixir--project-settings
      (setq lsp-elixir--project-settings
            (or (and (file-exists-p (lsp-elixir--config-file-path))
                     (lsp-elixir--project-read-file))
                (lsp-elixir--project-init-settings-file)))))

(defun lsp-elixir--project-read-file ()
  "Private function to load the project settings from the cache file."
  (with-temp-buffer
    (insert-file-contents (lsp-elixir--config-file-path))
    (goto-char (point-min)) (read (current-buffer))))

(defun lsp-elixir--project-init-settings-file ()
  "Private function to initialize the cache file."
  (lsp-elixir--project-save-settings (lsp-elixir--default-project-settings-map)))

(defun lsp-elixir--project-save-settings (project-settings-map)
  "Private function to save the current project settings to the cache file.
Argument PROJECT-SETTINGS-MAP is the current set of project settings to be saved to the cache file."
  (with-temp-file (lsp-elixir--config-file-path)
    (prin1 project-settings-map (current-buffer))))

(defun lsp-elixir--default-project-settings-map ()
  "Private function to initialize the cache data."
  (let ((default-map (make-hash-table :test 'equal)))
    (puthash "lsp-elixir-projects-version" 1 default-map)
    (puthash "lsp-elixir-projects" (make-hash-table :test 'equal) default-map)
    default-map))

(defun lsp-elixir--config-file-path ()
  "Private function to point to the hardcoded project settings file."
  (locate-user-emacs-file "lsp-elixir-project-settings.el"))

;; additional behavior

(defun lsp-elixir-macro-expand (start-pos end-pos)
  "Expands the selected code once.

This function has some string manipulation logic because elixir_sense returns
a string that begins and ends with parens, so we get rid of them to print
something meaningful to the user.
Argument START-POS is the beginning of the region.
Argument END-POS is the end of the region."
  (interactive "r")
  (lsp--cur-workspace-check)
  (let* ((selected-code (buffer-substring-no-properties start-pos end-pos))
         (response (lsp-send-request
                    (lsp-make-request
                     "elixirDocument/macroExpansion"
                     `(:context (:selection ,selected-code)
                                :position ,(lsp--cur-position)
                                :textDocument ,(lsp--make-text-document-item)))))
         (expansion (gethash "expand" response))
         (lines (cdr (butlast (split-string expansion "\n"))))
         (insertable (string-join
                      (mapcar (lambda (x) (concat "# " x)) lines)
                      "\n")))
    (save-excursion (goto-char start-pos)
                    (forward-line -1)
                    (insert insertable))))

(provide 'lsp-elixir)

;;; lsp-elixir.el ends here
