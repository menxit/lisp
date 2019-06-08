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

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select-by-property (property value)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd property) value)) *db*))

(defun dump-sql (p)
   (select-by-property (getf p :where) (getf p :equal)))

(defun add-cds ()
  (loop (prompt-add-cd)
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))