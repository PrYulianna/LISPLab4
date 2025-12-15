(defun find-min-with-key (lst key test)
    (let* ((first-elem (first lst))
        (first-key (funcall key first-elem)))
    (if (null (rest lst))
        (values first-elem first-key)
        (multiple-value-bind (rest-min rest-key) 
            (find-min-with-key (rest lst) key test)
            (if (funcall test first-key rest-key)
                (values first-elem first-key)
                (values rest-min rest-key))))))

(defun remove-first-by-key (elem lst key)
    (let ((target-key (funcall key elem)))
    (cond
        ((null lst) nil)
        ((equal (funcall key (first lst)) target-key) (rest lst))
        (t (cons (first lst) (remove-first-by-key elem (rest lst) key))))))

(defun selection-sort-functional (lst &key (key #'identity) (test #'<))
    (if (null lst)
        nil
        (multiple-value-bind (min-elem min-key) 
            (find-min-with-key lst key test)
            (cons min-elem 
                (selection-sort-functional 
                (remove-first-by-key min-elem lst key)
                :key key 
                :test test)))))

(defun check-first-function (name input expected &rest sort-args)
    (format t "~a ~:[FAILED~;passed~]~%"
        name
        (equal (apply #'selection-sort-functional input sort-args) expected)))

(defun test-first-function ()
    (check-first-function "[Test 1]" '(8 3 7 1 9) '(1 3 7 8 9))  
    (check-first-function "[Test 2]" '(15 10 5) '(5 10 15)) 
    (check-first-function "[Test 3]" '(4 4 4) '(4 4 4))
    (check-first-function "[Test 4]" '(6 2 6 2 1) '(1 2 2 6 6))
    (check-first-function "[Test 5]" '(100) '(100))
    (check-first-function "[Test 6]" '(-3 7 -1 4) '(-1 -3 4 7) :key #'abs)
    (check-first-function "[Test 7]" '(2 7 4 9 3) '(9 7 4 3 2) :test #'>))

(defun duplicate-elements-fn (n &key (duplicate-p (lambda (x) t)))
    (lambda (elem)
    (if (funcall duplicate-p elem)
        (make-list n :initial-element elem)
        (list elem))))

(defun check-second-function (name input expected)
    (format t "~a ~:[FAILED~;passed~]~%"
        name
        (equal input expected)))

(defun test-second-function ()
    (check-second-function "[Test 1]" (mapcan (duplicate-elements-fn 3) '(a b)) '(a a a b b b))
    (check-second-function "[Test 2]" (mapcan (duplicate-elements-fn 2 :duplicate-p #'oddp) '(2 3 4 5)) '(2 3 3 4 5 5))
    (check-second-function "[Test 3]" (mapcan (duplicate-elements-fn 4 :duplicate-p #'evenp) '(1 2 3)) '(1 2 2 2 2 3)))
    
(test-first-function)
(test-second-function)