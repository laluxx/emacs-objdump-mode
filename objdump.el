;;; objdump.el --- Disassemble and browse code -*- lexical-binding: t -*-

;; Author: raeburn

;; Keywords: tools

;;; Code:

(require 'cl-lib)

;;; Faces

(defgroup objdump nil
  "Major mode for viewing object file disassembly."
  :group 'tools)

(defface objdump-address-face
  '((t :inherit font-lock-constant-face))
  "Face for memory addresses."
  :group 'objdump-faces)

(defface objdump-symbol-face
  '((t :inherit font-lock-function-name-face))
  "Face for symbol names."
  :group 'objdump-faces)

(defface objdump-instruction-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for assembly instructions."
  :group 'objdump-faces)

(defface objdump-register-face
  '((t :inherit font-lock-variable-name-face))
  "Face for CPU registers."
  :group 'objdump-faces)

(defface objdump-immediate-face
  '((t :inherit font-lock-constant-face))
  "Face for immediate values."
  :group 'objdump-faces)

(defface objdump-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments and file info."
  :group 'objdump-faces)

;;; Variables

(defvar objdump-file-name nil
  "Name of object file currently being examined with objdump-mode, if any.")
(make-variable-buffer-local 'objdump-file-name)

(defvar objdump-symbol-table nil
  "Symbol table for current buffer.")
(make-variable-buffer-local 'objdump-symbol-table)

(defcustom objdump-command "objdump"
  "Command to run to disassemble object file"
  :type 'string
  :group 'objdump)

(defvar objdump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'imenu)
    (define-key map "i" 'imenu)
    (define-key map "g" 'objdump-revert)
    (define-key map "k" 'objdump-previous-function)
    (define-key map "j" 'objdump-next-function)
    (define-key map "p" 'objdump-previous-function)
    (define-key map "n" 'objdump-next-function)
    (define-key map "q" 'kill-this-buffer)
    map)
  "Keymap for `objdump-mode'.")

(defvar objdump-extensions
  '(".o"                                ; compiled object file
    ".so"                               ; shared library
    ".a"                                ; archive library
    ".ko"                               ; Linux kernel objects
                                        ;".dylib"                           ; Mac OS X libraries
                                        ;".dll"                             ; etc
                                        ;".obj"                             ; 
    )
  "Extensions typically indicating object files we should disassemble.")

(defvar objdump-font-lock-keywords
  `(
    ;; Addresses at start of line
    ("^ *\\([0-9a-f]+\\):" 1 'objdump-address-face)
    
    ;; Symbol definitions and references
    ("^[0-9a-f]+ <\\([^>]+\\)>:$" 1 'objdump-symbol-face)
    ("<\\([^>]+\\)>" 1 'objdump-symbol-face)
    
    ;; Instructions - note the pattern now requires a tab and whitespace
    ("\t[0-9a-f ]+\t\\([a-z][a-z0-9.]*\\)" 1 'objdump-instruction-face)
    
    ;; x86/x86_64 registers
    (,(concat "\\b\\(%[a-z][a-z0-9]*\\|[re][abcd]x\\|[re]sp\\|[re]bp\\|"
              "[re]si\\|[re]di\\|[re]ip\\|r[0-9]+\\|[xyz]mm[0-9]+\\)\\b")
     . 'objdump-register-face)
    
    ;; Immediate values
    ("\\b\\(\\$?-?0x[0-9a-fA-F]+\\|\\$[0-9]+\\)\\b" . 'objdump-immediate-face)
    
    ;; Comments and file info
    ("\\(#.*\\|File .*\\|\\.?\\.?L[A-Za-z0-9]*:\\)" . 'objdump-comment-face))
  "Syntax highlighting rules for objdump mode.")

;;; Helpers for address conversion

(defun convert-64bit-to-number (string)
  "Convert 64-bit hex STRING to number, handling sign extension."
  (if (and (eq (length string) 16)
           (string-match "^ff" string))
      (- (string-to-number (substring string 2) 16)
         (expt 2 56))
    (string-to-number string 16)))

(defun convert-number-to-64bit (number)
  "Convert NUMBER to 64-bit hex string, handling sign extension."
  (if (< number 0)
      (format "ff%14x" (+ number (expt 2 56)))
    (format "%x" number)))

;;; Symbol table management

(defun objdump--get-symbols ()
  "Get or build symbol table for current buffer."
  (or objdump-symbol-table
      (progn
        (setq objdump-symbol-table (make-vector 300 nil))
        (save-excursion
          (save-match-data
            (goto-char (point-min))
            (while (re-search-forward "^[0-9a-f]+ <\\([a-zA-Z_0-9:.]+\\)>:$"
                                      nil t)
              (intern (match-string 1) objdump-symbol-table))))
        objdump-symbol-table)))

(defun objdump--read-address (prompt)
  "Read an address with completion using PROMPT."
  (completing-read prompt (objdump--get-symbols) nil))

;;; Interactive commands

(defun objdump-revert ()
  "Rerun objdump on the (presumably changed) object file."
  (interactive)
  (if (not objdump-file-name)
      (error "No defined object file name for this buffer"))
  (let ((pos (point)))
    (objdump objdump-file-name)
    (goto-char pos)))

(defun get-completion-ignored-extensions-for-objects ()
  "Get completion ignored extensions, excluding object file extensions."
  (let ((extensions (apply 'list completion-ignored-extensions)))
    (mapc (lambda (e)
            (setq extensions (delete e extensions)))
          objdump-extensions)
    (append (list ".c" ".s" ".h" ".cc") extensions)))

;; Movement Functions

(defun objdump-next-function ()
  "Move to the start of the next function in objdump output."
  (interactive)
  (let ((old-point (point)))
    (end-of-line)
    (if (re-search-forward "^[0-9a-f]+ <[^>]+>:$" nil t)
        (progn
          (goto-char (line-beginning-position))
          (recenter))
      (goto-char old-point)
      (message "No more functions"))))

(defun objdump-previous-function ()
  "Move to the start of the previous function in objdump output."
  (interactive)
  (let ((old-point (point)))
    (beginning-of-line)
    (if (re-search-backward "^[0-9a-f]+ <[^>]+>:$" nil t)
        (progn 
          (goto-char (line-beginning-position))
          (recenter))
      (goto-char old-point)
      (message "No previous functions"))))


;; Imenu support

(defgroup objdump-completion nil
  "Completion settings for objdump mode."
  :group 'objdump)

(defface objdump-completion-address
  '((t :inherit marginalia-documentation))
  "Face for objdump addresses in completion annotations."
  :group 'objdump-completion)

(defface objdump-completion-size
  '((t :inherit marginalia-size :weight bold))
  "Face for function size annotations."
  :group 'objdump-completion)

(defvar-local objdump--longest-symbol-length 0
  "Length of longest symbol name in current buffer.")

(defvar-local objdump--longest-addr-length 0
  "Length of longest address in current buffer.")

(defun objdump--compute-function-size (start-addr next-addr)
  "Compute function size from START-ADDR to NEXT-ADDR."
  (when (and start-addr next-addr)
    (- (string-to-number next-addr 16)
       (string-to-number start-addr 16))))

(defun objdump-imenu-create-index ()
  "Create imenu index for objdump buffer."
  (let ((index-alist '())
        (max-len 0)
        (max-addr-len 0)
        (prev-addr nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9a-f]+\\) <\\([^>]+\\)>:$" nil t)
        (let* ((addr (match-string 1))
               (name (match-string 2))
               (name-len (length name))
               (addr-len (+ 2 (length addr))) ; +2 for "0x" prefix
               (size (when prev-addr 
                       (objdump--compute-function-size prev-addr addr)))
               (location (point-marker)))
          (setq max-len (max max-len name-len)
                max-addr-len (max max-addr-len addr-len))
          (let ((completion-item name))
            (put-text-property 0 (length completion-item) 
                               'objdump-address addr completion-item)
            (when size
              (put-text-property 0 (length completion-item) 
                                 'objdump-size size completion-item))
            (push (cons completion-item location) index-alist))
          (setq prev-addr addr))))
    (setq objdump--longest-symbol-length (+ max-len 2)
          objdump--longest-addr-length (+ max-addr-len 2))
    (nreverse index-alist)))

(defun objdump-completion-annotator (cand)
  "Annotate imenu CAND with address and size info for marginalia."
  (when-let ((addr (get-text-property 0 'objdump-address cand)))
    (let* ((size (get-text-property 0 'objdump-size cand))
           (addr-str (format "0x%s" addr))
           (addr-padding (make-string 
                          (max 0 (- objdump--longest-addr-length (length addr-str))) 
                          ?\s)))
      (concat
       (make-string (max 0 (- objdump--longest-symbol-length (length cand))) ?\s)
       (propertize addr-str 'face 'objdump-completion-address)
       addr-padding
       "  "  ; Two spaces after address
       (when size
         (propertize (format "%d" size) 
                     'face 'objdump-completion-size))))))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry
               '(imenu objdump-completion-annotator marginalia-annotate-binding)))



;;;###autoload
(define-derived-mode objdump-mode text-mode "Objdump"
  "Major mode for viewing object file disassembly.
\\{objdump-mode-map}"
  (setq buffer-read-only t)
  (setq font-lock-defaults '(objdump-font-lock-keywords))
  (setq truncate-lines t)
  (setq imenu-create-index-function #'objdump-imenu-create-index)
  ;; Advice imenu to recenter after jumping
  (advice-add 'imenu :after
              (lambda (&rest _)
                (beginning-of-line)
                (recenter 0))))


;;;###autoload
(defun objdump (filename)
  "Run objdump to disassemble an object file, and invoke objdump-mode."
  (interactive
   (let ((completion-ignored-extensions
          (get-completion-ignored-extensions-for-objects)))
     (list (read-file-name "Object file to disassemble: "
                           nil nil t))))
  (let ((output-buffer (get-buffer-create (concat "*Objdump " filename "*")))
        (command (concat objdump-command " -dCSlr " filename)))
    (with-current-buffer output-buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    (message "Running %s ..." command)
    (shell-command command output-buffer)
    (with-current-buffer output-buffer
      (objdump-mode)
      (setq objdump-file-name filename))
    (message "%s... %s" command 
             (propertize "DONE" 'face '(:inherit success :weight bold)))))

(provide 'objdump)
;;; objdump.el ends here
