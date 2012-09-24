;;; -*- emacs-lisp -*-
;;; cqtest.el
;;; Contest mode --- Contest Logging Major Mode for Emacs
;;;   by ykitta.

;;; User definition variables.
;; Interface
(defvar cqtest-length-fields '(
  (no       .  4)
  (date     .  5)
  (time     .  5)
  (callsign . 10)
  (nr       .  7)
  (band     .  5)
  (mode     .  5)
  (points   .  1)
  (op       . 10)
  (pwr      .  1)
  ))
(defvar cqtest-field-length-fieldsep  1)

;; Default Values
(defvar cqtest-band-alist 
  '(("1.9") ("3.5") ("7") ("10") ("14") ("18") ("21") ("24")
    ("28") ("50") ("144") ("430") ("1200") ("2400") ("5.6G") ("10Gup"))
  "Selectable band alist of contest. (MHz)
Must be no-null.")
(defvar cqtest-mode-alist '(("CW") ("SSB") ("RTTY") ("FM")
			    ("AM") ("PSK31") ("Other"))
  "Selectable mode alist of contest.
Must be non-null")
(defvar cqtest-op-alist '()
  "Operator's list for completion when select operator.")
(defvar cqtest-pwr-alist '(("H") ("M") ("L") ("P"))
  "Selectable power alist of contest")

(defvar cqtest-multi-map nil
  "Multiplier map
It is defined at each contest setting file
and used at extended map `cqtest-multi-exmap'.")

;; Sample Contest Setteing File.
(load "uectest.el")
; (load "~/cqtest-mode/uectest.el")

;;; Default values (Semi-automatically detected.)
(defvar cqtest-current-qso-no 0
  "QSO Serial Number. (Used when CQ WPX Contest)")
(defvar cqtest-current-band (caar cqtest-band-alist)
  "Band which automatically inserted when new line(new QSO line).")
(defvar cqtest-current-mode (caar cqtest-mode-alist)
  "Mode which automatically inserted when new line(new QSO line).")
(defvar cqtest-current-op '()
  "Operator which automatically inserted when new line(new QSO line).")
(defvar cqtest-current-pwr (caar cqtest-pwr-alist)
  "Power which automatically inserted when new line(new QSO line).")
(defvar cqtest-current-readability "5"
  "Readability which automatically inserted when new line(new QSO line).")
(defvar cqtest-current-signal-strength "9"
  "Signal Strength which automatically inserted when new line(new QSO line).")
(defvar cqtest-current-tone "9"
  "Tone which automatically inserted when new line(new QSO line).")

(defvar cqtest-auto-modify-timestamp-flag t
  "Flag whether modify timestamp when decision QSO.")

;; Variables which Program automatically decide.
(defvar cqtest-absolute-length-fields '()) ; automatically set.
(defvar cqtest-multi-flag-map '()
  "Multiplier flag map
It is alist that have multiplier as key and flags (value)
whether the operators have already communicated.
`cqtest-set-multi-flag-map' generates automatically
from alist `cqtest-multi-map'.
flags: nil=non-communicated t=communicated")
(defvar cqtest-score-map '()
  "Score Map
It contain Score table implemented by alist.
Key is band that is generated from `cqtest-band-alist'.
Value is list '(QSO Points Multiplier)'")

;; misc tool functions
(if (null (fboundp 'cdddr))
    (defun cdddr (x) (cdr (cdr (cdr x)))))

(defun chomp (string)
  "Truncate string's space at beginning and end."
  (replace-regexp-in-string "^\\s-+\\|\\s-+$" "" string))

(defun copy-list (list)
  "Copy list recursively. (data included list also is copied.)"
  (cond ((null list) nil)
	((consp list) (cons (copy-list (car list))
			    (copy-list (cdr list))))
	(t list)))

(defun get-previous-list (inlist field)
  "Get previous field of list."
  (cond ((null inlist) nil)
	((null (cdr inlist)) nil)
	((eq (cadr inlist) field) (car inlist))
	(t (get-previous-list (cdr inlist) field))))

(defun cqtest-move-editing-line ()
  "Move beginning of editing line."
  ;; Editing line is last line.
  (goto-char (point-max))
  (beginning-of-line)
  )

(defun cqtest-set-absolute-length-fields ()
  "Make alist `cqtest-absolute-length-fields'
Make alist which holds absolute field length from beginning of line.
And this alist is generated `cqtest-length-fields'.
This function is only called when initialized."
  (defun cqtest-set-absolute-length-fields-inner (ilist n)
    (cond ((null ilist) nil)
	  (t
	   (cons (cons (caar ilist)
		       (+ n cqtest-field-length-fieldsep))
;		       (+ n (cdar ilist) cqtest-field-length-fieldsep))
		 (cqtest-set-absolute-length-fields-inner
		  (cdr ilist) (+ n (cdar ilist)
				 cqtest-field-length-fieldsep)))
)))
  (setq cqtest-absolute-length-fields
	(cqtest-set-absolute-length-fields-inner
	 cqtest-length-fields
	 0)))

(defun cqtest-set-multi-flag-map ()
  (defun cqtest-set-multi-flag-map-inner (ilist)
    (cond ((null ilist) '())
	  (t
	   (cons (cons (caar ilist)
		       (make-list (length cqtest-band-alist) nil))
		 (cqtest-set-multi-flag-map-inner (cdr ilist))))))
    (setq cqtest-multi-flag-map
	  (cqtest-set-multi-flag-map-inner cqtest-multi-map)))

(defun cqtest-get-record-length ()
  "Return length of QSO record"
  (+ (cdar (last cqtest-absolute-length-fields))
     (cdar (last cqtest-length-fields))))

(defun cqtest-fieldp (&optional p)
  "Find what field the cursor is belong to.
If P is non-nil, Calcurate P instead of (point)"
  (let* ((x)
	 (ls (copy-list cqtest-absolute-length-fields))
	 (ret))
    (if (null p)
	(setq x (- (point) (point-at-bol)))
      (setq x (- p (point-at-bol))))
    
    (mapcar (lambda (p) (setcdr p (- x (cdr p)))) ls)
    (dolist (p ls)
      (unless (< (cdr p) 0) (setq ret p)))
    (cond ((null ret) nil)
	  ((and (equal (car ret)
		       (caar (last cqtest-absolute-length-fields)))
		(> (cdr ret) 0)) nil)
	  (t (car ret)))))

(defun cqtest-mode ()
  "cqtest-mode --- Contest Logging Major Mode for Emacs
\\{cqtest-keymap}"
  (interactive)
  (setq major-mode 'cqtest-mode
	mode-name "Contest Logging Mode")

  ;; Set variables
  (cqtest-set-absolute-length-fields)
  (cqtest-set-multi-flag-map)
  (cqtest-qso-no-set (cqtest-qso-no-get-maxno))
  (cqsm-init)

  ;; Set keymaps
;  (defvar cqtest-keymap (make-sparse-keymap))
  (defvar cqtest-keymap (make-keymap))
  (define-key cqtest-keymap (kbd "C-m") 'cqtest-decision-qso)
  (define-key cqtest-keymap (kbd "C-j") 'cqtest-decision-qso)
  (define-key cqtest-keymap (kbd "C-i") 'cqtest-move-field) ; tab
  (define-key cqtest-keymap (kbd "\t") 'cqtest-move-field)
  (define-key cqtest-keymap (kbd "C-c b") 'cqtest-set-band)
  (define-key cqtest-keymap (kbd "C-c m") 'cqtest-set-mode)
  (define-key cqtest-keymap (kbd "C-c o") 'cqtest-set-op)
  (define-key cqtest-keymap (kbd "C-c p") 'cqtest-set-pwr)
  (define-key cqtest-keymap (kbd "C-c C-v m") 'cqtest-multimap-make-buffer)
  (define-key cqtest-keymap (kbd "C-c C-v s") 'cqtest-scoremap-make-buffer)
  (use-local-map cqtest-keymap)

  (let ((i ?a) (j (- ?A ?a)))
    (while (< i (1+ ?z))
      (define-key cqtest-keymap (char-to-string i)
	'cqtest-self-insert-command)
      (define-key cqtest-keymap (char-to-string (+ i j))
	'cqtest-self-insert-command)
      (setq i (1+ i))))
  (let ((i ?0))
    (while (< i (1+ ?9))
      (define-key cqtest-keymap (char-to-string i)
	'cqtest-self-insert-command)
      (setq i (1+ i))))
  (define-key cqtest-keymap (kbd ".") 'cqtest-self-insert-command)
  (define-key cqtest-keymap (kbd "?") 'cqtest-self-insert-command)
  (define-key cqtest-keymap (kbd "/") 'cqtest-self-insert-command)

  ;; font-lock (Coloring)
  (font-lock-add-keywords
   'cqtest-mode
   '(("^#.*$" . font-lock-comment-face)))
  (font-lock-fontify-buffer)
  )


(defun cqtest-self-insert-command (&optional n)
  "Like `self-insert-command'. Handle espicially
when the cursor is in callsign field and nr field"
  (interactive "p")
  (let ((cursor (cqtest-fieldp)))
    (cond ((or (equal cursor 'callsign) (equal cursor 'nr))
	   (if (< (- (point) (point-at-bol))
		  (+ (cdr (assoc cursor cqtest-length-fields))
		     (cdr (assoc cursor cqtest-absolute-length-fields))))
	       (progn
		 (when (= (char-after) ? )
		   (delete-char 1))
		 (setq last-command-char (upcase last-command-char))
		 (self-insert-command n))
	     (message "%s is too long" cursor)))
;	  (t )
	  )
  ;; TODO: overwrite in callsign field and number field
  ;; TODO: To handle in field without callsign and nr field 
    ))

;; Move column to absolute field place.
(defun cqtest-get-length-between-bol-field (field)
  (cdr (assoc field cqtest-absolute-length-fields)))

(defun cqtest-move-field ()
  (interactive)
  (let ((p (- (point) (point-at-bol))))
    (cond ((and (< p	; point == callsign
		   (cqtest-get-length-between-bol-field 'nr))
		(> p (1- (cqtest-get-length-between-bol-field
			  'callsign))))
	   (move-to-column (cqtest-get-length-between-bol-field
			    'nr)))
	(t 				; point >= nr || point == time
	   (move-to-column (cqtest-get-length-between-bol-field
			    'callsign))))))

(defun replace-at-with-string (pnt text &optional f)
  "Replace string where pointed.
In other words, delete region and insert text.
optional args 'f':
if 'f' == t : round region when over `(point-at-eol)`.
if 'f' ==nil: round region when over `(point-max)`."
  (save-excursion
    (let ((region-end (+ pnt (length text))))
      ;; TODO: handling when "pnt > point-at-eol || point-max".
      (cond ((and (equal f t) (> region-end (point-at-eol)))
	     (delete-region pnt (point-at-eol)))
	    ((and (equal f nil) (> region-end (point-max)))
	     (delete-region pnt (point-max)))
	    (t (delete-region pnt region-end)))
      (goto-char pnt)
      (insert text))))
;;     (overwrite-mode 'overwrite-mode-textual)
;;     (goto-char pnt)
;;     (insert text)
;;     (overwrite-mode nil)))

;;; QSO No. Operation
;;; ------------------------------------------------------------
(defun cqtest-insert-qso-no ()
  "Insert QSO No. to QSO No. field of current-line."
  (replace-at-with-string
   (+ (point-at-bol)
      (cqtest-get-length-between-bol-field 'no))
   (format
    (format "%% %dd" (cdr (assoc 'no cqtest-length-fields)))
    cqtest-current-qso-no) t))

(defun cqtest-qso-no-increment ()
  "Increment QSO No. Variable."
  (setq cqtest-current-qso-no (1+ cqtest-current-qso-no)))

(defun cqtest-qso-no-get-maxno ()
  "Get maximum QSO No. from current buffer.
It is called when open buffer."
  (defun cqtest-qso-no-get-maxno-inner-get-qsono-at-record ()
    "Return QSO No. at line that point exists."
    (let ((start (+ (point-at-bol)
		    (cdr (assoc 'no cqtest-absolute-length-fields))))
	  (width (cdr (assoc 'no cqtest-length-fields))))
      (if (> (point-max) (+ start width))
	  (string-to-number (buffer-substring-no-properties
			     start (+ start width))))
      0))
  (let ((i 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (/= (following-char) 35)	; bol != '#'
	    (setq i
		  (max i (cqtest-qso-no-get-maxno-inner-get-qsono-at-record))))
	(forward-line 1)
	))
    i))

(defun cqtest-qso-no-set (value)
  "Set QSO No. Variable."
  (setq cqtest-current-qso-no value))

;;; Time and Date insert operation
;;; ------------------------------------------------------------
(defun cqtest-insert-date (datestring)
  "Insert date formatted string to date field of current-line."
  (replace-at-with-string
   (+ (point-at-bol) (cqtest-get-length-between-bol-field 'date))
   datestring t))

(defun cqtest-insert-current-date ()
  "Insert current date to date field of current-line."
  (cqtest-insert-date (format-time-string "%m/%d" (current-time))))

(defun cqtest-insert-time (timestring)
  "Insert time formatted string to time field of current-line."
  (replace-at-with-string
   (+ (point-at-bol) (cqtest-get-length-between-bol-field 'time))
   timestring t))

(defun cqtest-insert-current-time ()
  "Insert current time to time field of current-line."
  (cqtest-insert-time (format-time-string "%H:%M" (current-time))))

(defun cqtest-insert-timestamp ()
  "insert time to date and time field of current line"
  (cqtest-insert-current-date)
  (cqtest-insert-current-time))

;;; Band-selection
;;; ------------------------------------------------------------
(defun cqtest-set-band-to-current-line (band)
  (replace-at-with-string
   (+ (point-at-bol) (cqtest-get-length-between-bol-field 'band))
   (format (format "%% %ds" (cdr (assoc 'band cqtest-length-fields)))
	   band) t))

(defun cqtest-set-band ()
  "Change the default band and current QSO's band."
  (interactive)
  (let* ((completion-ignore-case t)
	 (newband (completing-read "New band: " cqtest-band-alist nil 1)))
    (setq cqtest-current-band newband)
    (cqtest-set-band-to-current-line newband)))

;;; Mode-selection
;;; ------------------------------------------------------------
(defun cqtest-set-mode-to-current-line (mode)
  (replace-at-with-string
   (+ (point-at-bol) (cqtest-get-length-between-bol-field 'mode))
   (format (format "%% %ds" (cdr (assoc 'mode cqtest-length-fields)))
	   mode) t))

(defun cqtest-set-mode ()
  "Change the default mode and current QSO's mode."
  (interactive)
  (let* ((completion-ignore-case t)
	 (newmode (completing-read "New mode: " cqtest-mode-alist nil 1)))
    (setq cqtest-current-mode newmode)
    (cqtest-set-mode-to-current-line newmode)))

;;; Points operation
;;; ------------------------------------------------------------
(defun cqtest-set-points-to-current-line (points)
  (replace-at-with-string
   (+ (point-at-bol) (cqtest-get-length-between-bol-field 'points))
   (format (format "%%-%ds" (cdr (assoc 'points cqtest-length-fields)))
	   points) t))

;;; Operator-selection
;;; ------------------------------------------------------------
(defun cqtest-set-op-to-current-line (op)
  (replace-at-with-string
   (+ (point-at-bol) (cqtest-get-length-between-bol-field 'op))
   (format (format "%%-%ds" (cdr (assoc 'op cqtest-length-fields)))
	   op) t))

(defun cqtest-set-op ()
  "Change the default operator and current QSO's operator."
  (interactive)
  (let* ((completion-ignore-case t)
	 (newop (completing-read "New operator: " cqtest-op-alist nil nil)))
    (setq cqtest-current-op newop)
    (cqtest-set-op-to-current-line newop)))

;;; Power-selection
;;; ------------------------------------------------------------
(defun cqtest-set-pwr-to-current-line (pwr)
  (replace-at-with-string
   (+ (point-at-bol) (cqtest-get-length-between-bol-field 'pwr))
   (format (format "%%-%ds" (cdr (assoc 'pwr cqtest-length-fields)))
	   pwr) t))

(defun cqtest-set-pwr ()
  "Change the default power and current QSO's operator."
  (interactive)
  (let* ((completion-ignore-case t)
	 (newpwr (completing-read "New power: " cqtest-pwr-alist nil nil)))
    (setq cqtest-current-pwr newpwr)
    (cqtest-set-pwr-to-current-line newpwr)))

;;; Multiplier map
;;; [Prefix] cqmm-*
;;; ------------------------------------------------------------
(defvar cqmm-buffer-name "*Multiplier map*")
(defvar cqmm-fields-length-alist '((key . 6) (value . 20)))
;; (defvar cqmm-multivalue-alist
;;   '()
;;   "")
;; (defun cqmm-init-multivalue-alist ()
;;   "Set value to `cqtest-multimap-multivalue-alist'
;; from value `cqtest-band-alist'. so this function should
;; be called after calling `cqtest-band-alist' initialization."
;;   (defun cqmm-init-multivalue-alist-inner (band-alist)
;;     (cons (caar band-alist) )
;;   ())

(defun cqmm-set-multivalue-with-band (record band value)
  "Set multiplier value from `cqtest-multi-flag-map'.
Key is band.
Example in `cqtest-multi-flag-map':
  ((multi-a . '(t t nil ... nil)) ... )
Each value of alist is related with `cqtest-band':
  ((\"3.5\") (\"7\") ... ) ; `cqtest-band'
   (t        t       ... ) ; value of `cqtest-multi-flag-map'"
  (defun cqmm-set-multivalue-with-band-inner (record band-alist band value)
    (cond ((string= (caar band-alist) band)
	   (setcar record value)) ; band-alist and band is string
	  ((null record) nil)
	  ((null band-alist) nil)
	  (t (cqmm-set-multivalue-with-band-inner (cdr record) (cdr band-alist) band value))))
  (cqmm-set-multivalue-with-band-inner record cqtest-band-alist band value))

(defun cqmm-get-multivalue-with-band (record band)
  "Get multiplier value from `cqtest-multi-flag-map'.
Key is band.
Example in `cqtest-multi-flag-map':
  ((multi-a . '(t t nil ... nil)) ... )
Each value of alist is related with `cqtest-band':
  ((\"3.5\") (\"7\") ... ) ; `cqtest-band'
   (t        t       ... ) ; value of `cqtest-multi-flag-map'"
  (defun cqmm-get-multivalue-with-band-inner (record band-alist band)
    (cond ((string= (caar band-alist) band)
	   (car record)) ; band-alist and band is string
	  ((null record) nil)
	  ((null band-alist) nil)
	  (t (cqmm-get-multivalue-with-band-inner (cdr record) (cdr band-alist) band))))
  (cqmm-get-multivalue-with-band-inner record cqtest-band-alist band))

(defvar cqmm-multiplier-sign-none "-")
(defvar cqmm-multiplier-sign-done "*")
(defun cqmm-make-string-multiplier-value (record)
  "Make string from multiplier flags."
  (cond ((null record) "")
	((null (car record))
	 (concat cqmm-multiplier-sign-none
		 (cqmm-make-string-multiplier-value (cdr record))))
	(t
	 (concat cqmm-multiplier-sign-done
		 (cqmm-make-string-multiplier-value (cdr record))))))

(defun cqmm-output-format ()
  "Format of multiplier buffer"
  (format "%%-%ds %%-%ds %%s\n"
	  (cdr (assoc 'key   cqmm-fields-length-alist))
	  (cdr (assoc 'value cqmm-fields-length-alist))))

(defun cqmm-set-multi-value (multi-key multi-value)
  (setcdr (assoc multi-key cqtest-multi-flag-map) multi-value))

;(defun cqtest-multi-map-output (multi-map flag-map)
(defun cqmm-output-string (multi-map flag-map)
  "Make formatted multiplier map string."
  (cond ((null multi-map) "")
	((null flag-map) "")
	(t (concat
	    (format (cqmm-output-format)
		    (caar multi-map) (cdar multi-map)
		    (cqmm-make-string-multiplier-value
		     (cdr (assoc (caar multi-map) flag-map))))
	    (cqmm-output-string (cdr multi-map) flag-map)))))

(defun cqmm-data-output ()
  "Output data of Multiplier map"
  (princ "Multiplier               Chkd.\n")
  (princ "------------------------------\n")
  (princ (cqmm-output-string cqtest-multi-map cqtest-multi-flag-map)))

(defun cqmm-refresh ()
  "Set Multiplier information to multiplier buffer."
  (save-excursion
    (set-buffer (get-buffer-create cqmm-buffer-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (cqmm-data-output))))

(defun cqtest-multimap-make-buffer ()
  "Show Multiplier buffer."
  (interactive)
  (with-output-to-temp-buffer cqmm-buffer-name
    (cqmm-data-output)))

;;; Score
;;; [Prefix] cqs-*
;;; ------------------------------------------------------------

;;; Score map
;;; [Prefix] cqsm-*
;;; ------------------------------------------------------------
;; cqtest-score-map
;; '((band . (QSO Points Multiplier)) ... )
(defun cqsm-init ()
  (defun cqsm-init-inner (band)
    (if (null band)
	nil
      (cons (cons (caar band) (list 0 0 0))
	    (cqsm-init-inner (cdr band)))))
  (setq cqtest-score-map (cqsm-init-inner cqtest-band-alist)))

;; cqsm-* getter method
(defun cqsm-get-qso-field (band)
  (nth 0 (cdr (assoc band cqtest-score-map))))
(defun cqsm-get-point-field (band)
  (nth 1 (cdr (assoc band cqtest-score-map))))
(defun cqsm-get-multiplier-field (band)
  (nth 2 (cdr (assoc band cqtest-score-map))))

;; cqsm-* setter method
(defun cqsm-set-qso-field (band value)
  (setcar (cdr (assoc band cqtest-score-map)) value))
(defun cqsm-set-point-field (band value)
  (setcar (cddr (assoc band cqtest-score-map)) value))
(defun cqsm-set-multiplier-field (band value)
  (setcar (cdddr (assoc band cqtest-score-map)) value))

;; cqsm-* add method
(defun cqsm-add-qso-field (band add-value)
  (cqsm-set-qso-field band (+ (cqsm-get-qso-field band) add-value)))
(defun cqsm-add-point-field (band add-value)
  (cqsm-set-point-field band (+ (cqsm-get-point-field band) add-value)))
(defun cqsm-add-multiplier-field (band add-value)
  (cqsm-set-multiplier-field band (+ (cqsm-get-multiplier-field band) add-value)))

;; calc with func `cqtest-get-point-from-qso'
;(defun cqsm-recalc-)

(defun cqsm-output-format ()
  "Format of score map buffer"
  (format "%%-%ds %% %dd %% %dd %% %dd\n"
	  (cdr (assoc 'band cqsm-fields-length-alist))
	  (cdr (assoc 'qso cqsm-fields-length-alist))
	  (cdr (assoc 'points cqsm-fields-length-alist))
	  (cdr (assoc 'multi cqsm-fields-length-alist))))

(defun cqsm-output-header ()
  "Header format of score map buffer"
  (format
   (format "%%-%ds %%-%ds %%-%ds %%-%ds\n"
	   (cdr (assoc 'band cqsm-fields-length-alist))
	   (cdr (assoc 'qso cqsm-fields-length-alist))
	   (cdr (assoc 'points cqsm-fields-length-alist))
	   (cdr (assoc 'multi cqsm-fields-length-alist)))
   "BAND" "QSO" "Pts." "Mult"))

(defun cqsm-output-string (cqs-map)
  "Make formatted score map string."
  (cond ((null cqs-map) "")
	(t (concat
	    (format (cqsm-output-format)
		    (nth 0 (car cqs-map))
		    (nth 1 (car cqs-map))
		    (nth 2 (car cqs-map))
		    (nth 3 (car cqs-map)))
	    (cqsm-output-string (cdr cqs-map))))))

(defun cqsm-record-length ()
  "Length of score map record"
  (+ (apply '+ (mapcar 'cdr cqsm-fields-length-alist)) 
     3)					; field separator
  )

(defun cqsm-total-format ()
  "Total format of score map buffer"
  (let ((label "TOTAL "))
    (format "%s%% %dd" label
	    (- (cqsm-record-length) (string-width label)))))

(defvar cqsm-buffer-name "*Score map*")
(defvar cqsm-fields-length-alist
  '((band . 6) (qso . 4) (points . 4) (multi . 4)))

(defun cqsm-data-output ()
  "Output data of Scoremap"
  (princ "Score map\n")
  (princ (cqsm-output-header))
  (princ "---------------------\n")
  (princ (cqsm-output-string cqtest-score-map))
  (princ "---------------------\n")
  (princ (format (cqsm-total-format) (cqsm-calc-score))))

(defun cqsm-refresh ()
  "Set Scoremap information to scoremap buffer."
  (save-excursion
    (set-buffer (get-buffer-create cqsm-buffer-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (cqsm-data-output))))

(defun cqtest-scoremap-make-buffer ()
  "Show Score map buffer."
  (interactive)
  (with-output-to-temp-buffer cqsm-buffer-name
    (cqsm-data-output)))

(defun cqtest-new-multi-p (multi band)
  "Return whether multiplier is new."
  (let ((record (cdr (assoc multi cqtest-multi-flag-map))))
    (eq (cqmm-get-multivalue-with-band record band) nil)))

(defun cqtest-on-invalid-nr ()
  "Called from `cqtest-on-decision-qso' when number of record is invalid.
Inner save-excursion."
  (move-to-column (+ (cdar (last cqtest-absolute-length-fields))
		     (cdar (last cqtest-length-fields)) 1) t)
  (insert "# Invalid NR"))

(defun cqtest-on-new-multi (multi-flag multi band)
  "Called when new multiplier
parameter `multi-flag' is multi-flag-map element such as:
  '(\"101\" . (t nil nil nil t nil))'"
  (cqsm-set-multiplier-field band (1+ (cqsm-get-multiplier-field band)))
  (cqmm-set-multivalue-with-band (cdr multi-flag) band t))

(defun cqtest-on-decision-qso ()
  "Called when decision QSO."
  ;; If needed, modify QSO Timestamp
  (if cqtest-auto-modify-timestamp-flag
      (cqtest-insert-timestamp))

  (save-excursion
    (let* ((cns (+ (point-at-bol)
		   (cdr (assoc 'no cqtest-absolute-length-fields))))
	   (ccs (+ (point-at-bol)
		   (cdr (assoc 'callsign cqtest-absolute-length-fields))))
	   (ces (+ (point-at-bol)
		   (cdr (assoc 'nr cqtest-absolute-length-fields))))
	   (cbs (+ (point-at-bol)
		   (cdr (assoc 'band cqtest-absolute-length-fields))))
	   (current-no (string-to-number 
			(buffer-substring-no-properties
			 cns (+ cns (cdr (assoc 'no
						cqtest-length-fields))))))
	   (current-call (chomp
			  (buffer-substring-no-properties
			   ccs (+ ccs (cdr (assoc 'callsign
						  cqtest-length-fields))))))
	   (current-nr (chomp
			(buffer-substring-no-properties
			 ces (+ ces (cdr (assoc 'nr
						cqtest-length-fields))))))
	   (current-band (chomp
			  (buffer-substring-no-properties
			   cbs (+ cbs (cdr (assoc 'band
						  cqtest-length-fields))))))
	   (multi (cqtest-get-multi current-no current-call current-nr))
 	   (pts (cqtest-get-point-from-qso current-call current-nr)))

      ;; Get multi this QSO
      ;; If multiplier of QSO is new multiplier:
      (let ((multi-flag (assoc multi cqtest-multi-flag-map)))
	(if (null multi-flag)
	    (cqtest-on-invalid-nr)
	  (if (cqtest-new-multi-p multi current-band)
	      (cqtest-on-new-multi multi-flag multi current-band)
	    )))

      ;; Update Score map (without multiplier)
      (cqsm-add-qso-field current-band 1)
      (cqsm-add-point-field current-band pts)

      ;; update points field
      (move-to-column (cdr (assoc 'points cqtest-absolute-length-fields)))
      (cqtest-set-points-to-current-line pts)
    ))

  ;; TODO: Refresh multiplier-map
  ;; TODO: Refresh Score.
  )

(defun cqtest-on-new-qsoline ()
  "Called when new line.
Set default value to field."
  (insert-char ?  (cqtest-get-record-length)) ; fill with space.
  (cqtest-insert-qso-no)
  (cqtest-insert-timestamp)
  (cqtest-set-band-to-current-line cqtest-current-band)
  (cqtest-set-mode-to-current-line cqtest-current-mode)
  (cqtest-set-op-to-current-line cqtest-current-op)
  (cqtest-set-pwr-to-current-line cqtest-current-pwr)
  )

;; C-j
(defun cqtest-decision-qso ()
  ""
  (interactive)
  ;; TODO: Do action when only current line
  
  (cqtest-on-decision-qso)

  (goto-char (point-at-eol))
  (unless (and (bolp) (eolp))
    (insert "\n"))
  (cqtest-qso-no-increment)
  (cqtest-on-new-qsoline)
  (move-to-column (cdr (assoc 'callsign cqtest-absolute-length-fields)))

  ;; refresh sub windows.
  (cqmm-refresh)
  (cqsm-refresh)

  (message "")
  )

(provide 'cqtest)
;;; EOF