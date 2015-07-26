(require 'epc)

(defvar english-epc (epc:start-epc "python" '("english-syntax-color.py")))

(deferred:$
  (epc:call-deferred english-epc 'parse (list "foo"))
  (deferred:nextc it
    (lambda (x) (message "Return : %S" x))))
(defun parse-sentence (message)
  (if message
      (epc:call-sync english-epc 'parse (list message) )
    nil)
)

(defun get-face (tag)
  (cond ((string= tag "ADJ") '(face english-syntax-adj))
        ((string= tag "ADP") '(face english-syntax-adp))
        ((string= tag "ADV") '(face english-syntax-adv))
        ((string= tag "CONJ") '(face english-syntax-conj))
        ((string= tag "DET") '(face english-syntax-det))
        ((string= tag "NOUN") '(face english-syntax-noun))
        ((string= tag "NUM") '(face english-syntax-num))
        ((string= tag "PRT") '(face english-syntax-prt))
        ((string= tag "PRON") '(face english-syntax-pron))
        ((string= tag "VERB") '(face english-syntax-verb))
        ((string= tag ".") '(face english-syntax-other))
        ((string= tag "X") '(face english-syntax-other))
        (t (progn
             (message (format "%s" "INVALID TAG FOUND?"))
             '(face english-syntax-other)))
        ))

(defun color-buffer (start list)
  (if list (progn
             (let* (
                   (word (car (car list)))
                   (tag (cadr (car list)))
                   (end (+ (length word) start))
                   (face-var (get-face tag)))
               (if (eq (+ end 1) (point-at-eol)) (setq end (+ end 1)))
               (set-text-properties start end face-var)
               (goto-char end)
               (skip-syntax-forward " ")
               ;(message (format "%s" start))
               ;(message (format "%s" end))
               ;(message (format "%s" word))
               ;(message (format "%s" tag))
               (color-buffer (point) (cdr list)))
             )
    )
  "fin")

(defun process-sentence-at-point ()
  (interactive)
  (let ((point (point)))
  (color-buffer (car (bounds-of-thing-at-point 'sentence)) (parse-sentence (thing-at-point 'sentence t)))
  (goto-char point)
  )
  )

(defun process-buffer-at-point ()
  (interactive)
  (let ((point (point)))
  (color-buffer 1 (parse-sentence (buffer-substring-no-properties 1 (buffer-size))))
  (goto-char point)
  )
  )

(defun process-word-at-point ()
  (interactive)
  (let ((point (point)))
    (message (format "%s" (thing-at-point 'word)))
    (color-buffer (car (bounds-of-thing-at-point 'word)) (parse-sentence (thing-at-point 'word t)))
    (goto-char point)
    )
  )
;(add-hook 'before-change-functions (lambda (x y) (process-sentence-at-point)))

;;Faces
(defface english-syntax-adj
  '((((class color) (min-colors 88) (background light))
     :foreground "brown")
    (((class color) (min-colors 88) (background dark))
     :foreground "brown")
    (((class color) (min-colors 16) (background light))
     :foreground "brown")
    (((class color) (min-colors 16) (background dark))
     :foreground "brown")
    (((class color) (min-colors 8))
     :foreground "brown")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)

(defface english-syntax-adp
  '((((class color) (min-colors 88) (background light))
     :foreground "blue")
    (((class color) (min-colors 88) (background dark))
     :foreground "blue")
    (((class color) (min-colors 16) (background light))
     :foreground "blue")
    (((class color) (min-colors 16) (background dark))
     :foreground "blue")
    (((class color) (min-colors 8))
     :foreground "blue")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)

(defface english-syntax-adv
  '((((class color) (min-colors 88) (background light))
     :foreground "green")
    (((class color) (min-colors 88) (background dark))
     :foreground "green")
    (((class color) (min-colors 16) (background light))
     :foreground "green")
    (((class color) (min-colors 16) (background dark))
     :foreground "green")
    (((class color) (min-colors 8))
     :foreground "green")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)

(defface english-syntax-conj
  '((((class color) (min-colors 88) (background light))
     :foreground "pink")
    (((class color) (min-colors 88) (background dark))
     :foreground "pink")
    (((class color) (min-colors 16) (background light))
     :foreground "pink")
    (((class color) (min-colors 16) (background dark))
     :foreground "pink")
    (((class color) (min-colors 8))
     :foreground "pink")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)

(defface english-syntax-det
  '((((class color) (min-colors 88) (background light))
     :foreground "purple")
    (((class color) (min-colors 88) (background dark))
     :foreground "purple")
    (((class color) (min-colors 16) (background light))
     :foreground "purple")
    (((class color) (min-colors 16) (background dark))
     :foreground "purple")
    (((class color) (min-colors 8))
     :foreground "purple")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)


(defface english-syntax-noun
  '((((class color) (min-colors 88) (background light))
     :foreground "grey")
    (((class color) (min-colors 88) (background dark))
     :foreground "grey")
    (((class color) (min-colors 16) (background light))
     :foreground "grey")
    (((class color) (min-colors 16) (background dark))
     :foreground "grey")
    (((class color) (min-colors 8))
     :foreground "grey")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)


(defface english-syntax-num
  '((((class color) (min-colors 88) (background light))
     :foreground "cyan")
    (((class color) (min-colors 88) (background dark))
     :foreground "cyan")
    (((class color) (min-colors 16) (background light))
     :foreground "cyan")
    (((class color) (min-colors 16) (background dark))
     :foreground "cyan")
    (((class color) (min-colors 8))
     :foreground "cyan")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)

(defface english-syntax-prt
  '((((class color) (min-colors 88) (background light))
     :foreground "magenta")
    (((class color) (min-colors 88) (background dark))
     :foreground "magenta")
    (((class color) (min-colors 16) (background light))
     :foreground "magenta")
    (((class color) (min-colors 16) (background dark))
     :foreground "magenta")
    (((class color) (min-colors 8))
     :foreground "magenta")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)


(defface english-syntax-pron
  '((((class color) (min-colors 88) (background light))
     :foreground "steel blue")
    (((class color) (min-colors 88) (background dark))
     :foreground "steel blue")
    (((class color) (min-colors 16) (background light))
     :foreground "steel blue")
    (((class color) (min-colors 16) (background dark))
     :foreground "steel blue")
    (((class color) (min-colors 8))
     :foreground "steel blue")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)


(defface english-syntax-verb
  '((((class color) (min-colors 88) (background light))
     :foreground "red")
    (((class color) (min-colors 88) (background dark))
     :foreground "red")
    (((class color) (min-colors 16) (background light))
     :foreground "red")
    (((class color) (min-colors 16) (background dark))
     :foreground "red")
    (((class color) (min-colors 8))
     :foreground "red")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)

(defface english-syntax-other
  '((((class color) (min-colors 88) (background light))
     :foreground "black")
    (((class color) (min-colors 88) (background dark))
     :foreground "black")
    (((class color) (min-colors 16) (background light))
     :foreground "black")
    (((class color) (min-colors 16) (background dark))
     :foreground "black")
    (((class color) (min-colors 8))
     :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'english-syntax-faces)

;;Macro for renaming these faces
(fset 'rename-face-color
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("lvt\"yhkkpdt\"uldt\"T\"hkkp\371\371ldt\"T\"hkkp\371\371\371ldt\"T\"hkkp\371\371\371\371ldt\"T\"h" 0 "%d")) arg)))
