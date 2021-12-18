((nil
  . ((eval . (progn
               (require 'fennel-mode)

               (defvar apiales-modules '((compiler . "compiler")
                                         (lexing . "compiler.lexing")
                                         (ast . "compiler.ast")
                                         (parsing . "compiler.parsing")
                                         (symbol-table . "compiler.symbol-table")
                                         (analysis . "compiler.analysis")
                                         (riscv . "compiler.riscv")
                                         (codegen . "compiler.codegen")
                                         (util . "compiler.util")
                                         (test . "tests"))
                 "Modules to load into the fennel repl.")

               (defvar apiales-mode-map (make-sparse-keymap)
                 "Keymap while apiales-mode is active.")

               (define-minor-mode apiales-mode
                 "A temporary minor mode to be activated only specific to a buffer."
                 :lighter " [apiales]"
                 :keymap apiales-mode-map)

               (add-hook 'apiales-mode-hook 'projectile-mode)

               (defun apiales-repl ()
                 (interactive)
                 (let* ((project (projectile-acquire-root))
                        (fennel-program (concat project "bin/fennel --repl"))
                        (repl-buf (get-buffer fennel-repl--buffer)))
                   (when repl-buf
                     (if (yes-or-no-p "Fennel REPL already open, kill it?")
                         (kill-buffer repl-buf)
                       (error "Cannot start new repl while already running.")))
                   (projectile-with-default-dir project
                     (fennel-repl nil))
                   (apiales-load-modules)))

               (defun apiales-load-modules ()
                 (interactive)
                 (let* ((fennel-proc (get-buffer-process fennel-repl--buffer)))
                   (pcase-dolist (`(,name . ,source) apiales-modules)
                     (comint-send-string
                      fennel-proc
                      (format "%s\n" `(local ,name (require ,(concat ":" source))))))))

               (defun apiales-reload-modules ()
                 (interactive)
                 (let ((fennel-proc (get-buffer-process fennel-repl--buffer)))
                   (pcase-dolist (`(,name . ,source) apiales-modules)
                     (comint-send-string fennel-proc (fennel-reload-form (concat ":" source)))))
                 (switch-to-lisp t))

               (define-key apiales-mode-map (kbd "C-c C-f") 'apiales-repl)
               (define-key apiales-mode-map (kbd "C-c C-r") 'apiales-reload-modules)

               (apiales-mode 1))))))
