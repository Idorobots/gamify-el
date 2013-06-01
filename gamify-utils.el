;; Various utilities

(require 'cl)

(defun gamify-format-expand (formatters format &optional arg)
  "Formats `format' according to `formatters' passing `arg' as an optional argument."
  (save-match-data
    (let ((regex (concat "%\\("
                         (reduce (lambda (a b)
                                   (concat a "\\|" b))
                                 (mapcar #'car formatters))
                         "\\)")))
      (replace-regexp-in-string regex
                                (lambda (str)
                                  (let ((fun (assoc (substring str 1)
                                                    formatters)))
                                    (if fun
                                        (funcall (cdr fun) arg)
                                        (error "Unrecognized format sequence: %s" str))))
                                format t t))))

(defun gamify-split-string-on-case (string &optional down-p)
  (let* ((len (length string))
         (start (- len 1))
         (end len)
         (matches ())
         (cas (if down-p
                  #'downcase
                  #'upcase)))
    (while (/= start -1)
      (let ((char (aref string start)))
        (when (= char (funcall cas char))
          (setq matches (cons (substring string start end)
                              matches))
          (setq end start)))
      (setq start (- start 1)))
    (if (/= end 0)
        (cons (substring string 0 end)
              matches)
        matches)))

(provide 'gamify-utils)