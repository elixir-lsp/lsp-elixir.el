(require 'lsp-imenu)
(require 'lsp-mode)

(defgroup lsp-elixir-server nil
  "Which Language Server Protocol lsp-elixir will use"
  :prefix "lsp-elixir-server-"
  :group 'lsp-elixir)

(defcustom lsp-elixir-server-extension nil
  "Help lsp-elixir decide if you are running on a *nix or Microsoft machine"
  :type '(choice (const :tag "Windows" bat)
                 (const :tag "*nix" sh))
  :group 'lsp-elixir-server)

(defconst lsp-elixir-server-root-path
  (concat (file-name-directory load-file-name) "elixir-ls/"))

(defvar lsp-elixir--project-settings nil
  "Where lsp-elixir keeps its project-level settings")

(lsp-define-stdio-client
 lsp-elixir-mode
 "elixir"
 (lambda () (lsp-elixir-project-root-or-default-dir))
 (lsp-elixir--lsp-server-path-for-current-project))
;; '("~/src/projects/lsp-elixir.el/elixir-ls/erl19/language_server.sh")

;; TODO what to do with these hooks?
;; (add-hook 'lsp-elixir-mode-hook 'lsp-ui-mode)
;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

(defun lsp-elixir-macro-expand (start-pos end-pos)
  "Expands the selected code once.

This function has some string manipulation logic because elixir_sense returns
a string that begins and ends with parens, so we get rid of them to print something
meaningful to the user."
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
                    (previous-line)
                    (insert insertable))))

(defun lsp-elixir--lsp-server-path-for-current-project ()
  `(,(concat lsp-elixir-server-root-path
           "erl"
           (lsp-elixir--server-erlang-version (lsp-elixir-project-root-or-default-dir))
           "/"
           "language_server"
           "."
           (lsp-elixir--server-extension))))

(defun lsp-elixir--server-erlang-version (project-path)
  (let* ((project-settings-map (lsp-elixir--project-settings))
         (project-erlang-version (or (gethash project-path
                                              (gethash "lsp-elixir-projects" project-settings-map))
                                     (completing-read "Choose which version of Erlang the LSP server should use: "
                                                      '("19" "20")
                                                      nil
                                                      t
                                                      ))))
    (puthash project-path project-erlang-version (gethash "lsp-elixir-projects" project-settings-map))
    (setq lsp-elixir--project-settings project-settings-map)
    (lsp-elixir--project-save-settings project-settings-map)
    project-erlang-version))

(defun lsp-elixir--server-extension ()
  (let ((extension (or lsp-elixir-server-extension
                       (completing-read "Choose the kind of executable that runs on this system: "
                                        '("sh" "bat")
                                        nil
                                        t
                                        ))))
    (setq lsp-elixir-server-extension extension)
    extension))

(defun lsp-elixir--project-settings ()
  (or lsp-elixir--project-settings
      (setq lsp-elixir--project-settings
            (or (and (file-exists-p (lsp-elixir--config-file-path))
                     (lsp-elixir--project-read-file))
                (lsp-elixir--project-init-settings-file)))))

(defun lsp-elixir--project-read-file ()
  (with-temp-buffer
    (insert-file-contents (lsp-elixir--config-file-path))
    (goto-char (point-min)) (read (current-buffer))))

(defun lsp-elixir--project-init-settings-file ()
  (lsp-elixir--project-save-settings (lsp-elixir--default-project-settings-map)))

(defun lsp-elixir--project-save-settings (project-settings-map)
  (with-temp-file (lsp-elixir--config-file-path)
    (prin1 project-settings-map (current-buffer))))

(defun lsp-elixir--default-project-settings-map ()
  (let ((default-map (make-hash-table :test 'equal)))
    (puthash "lsp-elixir-projects-version" 1 default-map)
    (puthash "lsp-elixir-projects" (make-hash-table :test 'equal) default-map)
    default-map))

(defun lsp-elixir--config-file-path ()
  (locate-user-emacs-file "lsp-elixir-project-settings.el"))

(provide 'lsp-elixir)
