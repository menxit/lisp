(defvar *db* nil)

(defun make-cd (title artist rating hd)
  (list :title title :artist artist :rating rating :hd hd))

(defun add-cd (cd)
  (push cd *db*))

(defun dump (db)
  (dolist (cd db)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun dump-db ()
  (dump *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "HD")))

(defun prompt-add-cd ()
  (add-cd (prompt-for-cd)))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key title artist rating (hd nil hd-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if hd-p (equal (getf cd :hd) hd) t))))

(defun add-cds ()
  (loop (prompt-add-cd)
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(select (where :artist "hel"))

(reverse '(1 2 3))

(defun add-two-or-three-numbers(&key one two three)
  (+ (if one one 0) (if two two 0) (if three three 0)))

(add-two-or-three-numbers :one 1 :two 2)
(add-two-or-three-numbers :one 1 :two 2 :three 10)
