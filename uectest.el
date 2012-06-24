;;; -*- emacs-lisp -*-
;;; uectest.el
;;; UEC Contest Definition File
;;; Sample contest definition file for cqtest.el
;;;   by ykitta.


(setq cqtest-band-alist '(("3.5") ("7") ("14") ("21") ("28") ("50")))
(setq cqtest-mode-alist '(("CW")))


(setq cqtest-multi-map 
      '(("101" . "宗谷") ("102" . "留萌") ("103" . "上川")
	("104" . "網走") ("105" . "空知") ("106" . "石狩")
	("107" . "根室") ("108" . "後志") ("109" . "十勝")
	("110" . "釧路") ("111" . "日高") ("112" . "胆振")
	("113" . "桧山") ("114" . "渡島") ("02" . "青森県")
	("03" . "岩手県") ("04" . "秋田県") ("05" . "山形県")
	("06" . "宮城県") ("07" . "福島県") ("08" . "新潟県")
	("09" . "長野県") ("10" . "東京都") ("11" . "神奈川県")
	("12" . "千葉県") ("13" . "埼玉県") ("14" . "茨城県")
	("15" . "栃木県") ("16" . "群馬県") ("17" . "山梨県")
	("18" . "静岡県") ("19" . "岐阜県") ("20" . "愛知県")
	("21" . "三重県") ("22" . "京都府") ("23" . "滋賀県")
	("24" . "奈良県") ("25" . "大阪府") ("26" . "和歌山県")
	("27" . "兵庫県") ("28" . "富山県") ("29" . "福井県")
	("30" . "石川県") ("31" . "岡山県") ("32" . "島根県")
	("33" . "山口県") ("34" . "鳥取県") ("35" . "広島県")
	("36" . "香川県") ("37" . "徳島県") ("38" . "愛媛県")
	("39" . "高知県") ("40" . "福岡県") ("41" . "佐賀県")
	("42" . "長崎県") ("43" . "熊本県") ("44" . "大分県")
	("45" . "宮崎県") ("46" . "鹿児島県") ("47" . "沖縄県")
	("48" . "小笠原") ("49" . "沖の鳥島") ("50" . "南鳥島")
	("00" . "MM")))

(setq cqtest-licence-list
      '("H" "I" "L" "UEC"))

(defun cqtest-get-multi (serialno callsign exnumber)
  "Return multiplier from QSO.
`serialno' is serialized number of contest QSOs. (for WPX)
`callsign' is callsign's field string.
`exnumber' is exchanged number excluded RST code."
  (when (string-match "\\([0-9][0-9]+\\)\\(UEC\\|H\\|I\\|L\\)"
			     exnumber)
    (let ((multi (match-string 1 exnumber))
	  (licence (match-string 2 exnumber)))
      multi
    )))

(defun cqtest-get-point-from-qso (callsign exnumber)
  "Return QSO's point"
  (if (or (null callsign) (string= callsign "")) 0
    (when (string-match "\\([0-9][0-9]+\\)\\(UEC\\|H\\|I\\|L\\)"
			exnumber)
      (let ((multi (match-string 1 exnumber))
	    (licence (match-string 2 exnumber)))
	(cond ((string= licence "UEC") 5)
	      ((string= licence "L") 4)
	      ((string= licence "I") 3)
	      ((string= licence "H") 2)
	      (t 0)			; invalid number
	)))))

(defun cqsm-calc-score ()
  "Calcurate Score from score map (`cqtest-score-map')"
  (let ((qso 0)
	(pts 0)
	(multi 0))
    (dolist (record cqtest-score-map)
      (setq qso (+ qso (nth 1 record)))
      (setq pts (+ pts (nth 2 record)))
      (setq multi (+ multi (nth 3 record))))
    
    ;; total score := pts * multi
    (* pts multi)))

;; EOF