;;;; JSON to ElasticSearch script

(with-open-file (stream "./fixed-restaurants-for-elastic.json" :direction :output :if-exists :supersede)
  (let ((in (open "./restaurants.json"
                  :if-does-not-exist (format t "~%doesn't exist")))
        (id 1))
    (when in
      (loop :for line = (read-line in nil)
            :while line
            :do (progn (format stream "{\"index\":{\"_index\": \"restaurants\",\"_type\":\"restaurant\",\"_id\":~a}}~%{\"fields\" : ~a}~%" id line)
                       (setf id (+ id 1))))
      (close in))))

(format t "~%done")
