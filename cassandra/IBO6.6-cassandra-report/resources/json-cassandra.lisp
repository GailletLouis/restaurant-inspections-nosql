;;;; JSON to cassandra script

(with-open-file (stream "~/Downloads/fixed-restaurants.txt" :direction :output :if-exists :supersede)
  (let ((in (open "~/Downloads/restaurants.json" :if-does-not-exist (format t "~%doesn't exist"))))
    (when in
      (loop :for line = (read-line in nil)
            :while line
            :do (format stream "INSERT INTO restaurants JSON '~a';~%" line))
      (close in))))

(format t "~%done")
