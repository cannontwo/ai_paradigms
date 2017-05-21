;; The Maze Searching Domain (pg. 134)

(in-package :gps)

;; Functions
(defun make-maze-ops (pair)
  "Make maze ops in both directions"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places."
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

;; pg. 19
(defun mappend (fn the-list)
  "Aplly fn to each element of list and append the results"
  (apply #'append (mapcar fn the-list)))

(defun find-path (start end)
  "Search a maze for a path from start to end."
  (let ((results (gps `((at ,start)) `((at ,end)))))
    (unless (null results)
      (cons start (mapcar #'destination
                          (remove '(start) results
                                  :test #'equal))))))

(defun destination (action)
  "Find the Y in (executing (move from X to Y))"
  (fifth (second action)))

;; Parameters
(defparameter *maze-ops*
  (mappend #'make-maze-ops
           '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
             (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
             (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))
