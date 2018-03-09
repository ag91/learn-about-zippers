(defun my-zipper/from-list (list)
  "creates a zipper from a list"
  `(nil ,list))

(defun my-zipper/element (zipper)
  "returns current zipper element"
  (let ((right (second zipper)))
    (if right (first right) (first (reverse (first zipper))))))

(defun my-zipper/right (zipper)
  "returns zipper shifted to right"
  (let ((right (second zipper))
        (left (first zipper)))
    (if right
      (if left
        (list (append left (list (first right))) (cdr right))
        (list (list (first right)) (cdr right))) 
      zipper)))

(defun my-zipper/left (zipper)
  "returns zipper shifted to left"
  (let ((right (second zipper))
        (left (first zipper)))
    (if left
      (if right
        (list (butlast left) (cons (first (reverse (first zipper))) right))
        (list (butlast left) (list (first (reverse (first zipper))))))
      zipper)))

(defun my-zipper/up (zipper)
  "returns zipper shifted up"
  ;; TODO
  zipper)

(defun my-zipper/down (zipper)
  "returns zipper shifted down"
  ;; TODO
  zipper)

(ert-deftest zipper-from-list ()
  (let ((my-zipper (my-zipper/from-list '(1 2 3 4))))
    (should (equal my-zipper '(nil (1 2 3 4))))))

(ert-deftest zipper-element ()
  (let ((my-zipper (my-zipper/from-list '(1 2 3 4))))
    (should (equal (my-zipper/element my-zipper) 1))))

(ert-deftest zipper-element-at-end ()
  (let ((my-zipper (my-zipper/right (my-zipper/from-list '(1)))))
    (should (equal (my-zipper/element my-zipper) 1))))

(ert-deftest zipper-right ()
  (let ((my-zipper (my-zipper/from-list '(1 2 3 4))))
    (should (equal (my-zipper/right my-zipper) '((1) (2 3 4))))))

(ert-deftest zipper-right-at-end ()
  (let ((my-zipper (my-zipper/from-list '(1))))
    (should (equal (my-zipper/right my-zipper) '((1) nil)))))

(ert-deftest zipper-left ()
  (let ((my-zipper (my-zipper/right (my-zipper/from-list '(1 2)))))
    (should (equal (my-zipper/left my-zipper) '(nil (1 2))))))

(ert-deftest zipper-left-at-beginning ()
  (let ((my-zipper (my-zipper/from-list '(1))))
    (should (equal (my-zipper/left my-zipper) '(nil (1))))))


(ert-run-tests-interactively 't)

(defun my-better-zipper/from-list (list)
  "creates a zipper from a list"
  `(nil ,list))

(defun my-better-zipper/element (zipper)
  "returns current zipper element"
  (let ((right (second zipper)))
    (if right (first right) (first (first zipper)))))

(defun my-better-zipper/right (zipper)
  "returns zipper shifted to right"
  (let ((right (second zipper))
        (left (first zipper)))
    (if right
      (if left
        (list (cons (first right) left) (cdr right))
        (list (list (first right)) (cdr right))) 
      zipper)))

(defun my-better-zipper/left (zipper)
  "returns zipper shifted to left"
  (let ((right (second zipper))
        (left (first zipper)))
    (if left
      (if right
        (list (cdr left) (cons (first left) right))
        (list (cdr left) (list (first left))))
      zipper)))

(defun my-better-zipper/up (zipper)
  "returns zipper shifted up"
  ;; TODO
  zipper)

(defun my-better-zipper/down (zipper)
  "returns zipper shifted down"
  ;; TODO
  zipper)

(ert-deftest zipper-from-list ()
  (let ((my-better-zipper (my-better-zipper/from-list '(1 2 3 4))))
    (should (equal my-better-zipper '(nil (1 2 3 4))))))

(ert-deftest zipper-element ()
  (let ((my-better-zipper (my-better-zipper/from-list '(1 2 3 4))))
    (should (equal (my-better-zipper/element my-better-zipper) 1))))

(ert-deftest zipper-element-at-end ()
  (let ((my-better-zipper (my-better-zipper/right (my-better-zipper/from-list '(1)))))
    (should (equal (my-better-zipper/element my-better-zipper) 1))))

(ert-deftest zipper-right ()
  (let ((my-better-zipper (my-better-zipper/from-list '(1 2 3 4))))
    (should (equal (my-better-zipper/right my-better-zipper) '((1) (2 3 4))))))

(ert-deftest zipper-right-at-end ()
  (let ((my-better-zipper (my-better-zipper/from-list '(1))))
    (should (equal (my-better-zipper/right my-better-zipper) '((1) nil)))))

(ert-deftest zipper-left ()
  (let ((my-better-zipper (my-better-zipper/right (my-better-zipper/from-list '(1 2)))))
    (should (equal (my-better-zipper/left my-better-zipper) '(nil (1 2))))))

(ert-deftest zipper-left-at-beginning ()
  (let ((my-better-zipper (my-better-zipper/from-list '(1))))
    (should (equal (my-better-zipper/left my-better-zipper) '(nil (1))))))


(ert-run-tests-interactively 't)

(defmacro chrono-func (output fn &rest args)
  `(let ((init-time (cadr (current-time)))
         (final-time)
         (final-result))
     (setq final-result (funcall ,fn ,@args))
     (setq final-time (- (cadr (current-time)) init-time))
     (message "Time:%2s s" final-time)
     (when ,output
       final-result)))

(defmacro chrono-func (output fn &rest args)
    `(let ((init-time (cadr (current-time)))
           (final-time)
           (final-result))
       (setq final-result (funcall ,fn ,@args))
       (setq final-time (- (cadr (current-time)) init-time))
       (message "Time:%2s s" final-time)
       (if ,output
         final-result
         final-time)))

(let* ((n 10000)
       (list (number-sequence 0 n))
       (my-zipper (my-zipper/from-list list))
       (my-better-zipper (my-better-zipper/from-list list))
       (measure-zipper (lambda (zipper right left) 
                         (chrono-func nil 
                           (lambda () 
                             (let ((my-zip zipper))
                               ; go right
                               (while (second my-zip)
                                 (setq my-zip (funcall right my-zip)))
                               ; go left
                               (while (first my-zip)
                                 (setq my-zip (funcall left my-zip)))))))))
  (funcall measure-zipper my-zipper 'my-zipper/right 'my-zipper/left)
  (funcall measure-zipper my-better-zipper 'my-better-zipper/right 'my-better-zipper/left))

(defun my-real-zipper/from-list (list)
  "creates a zipper from a list"
  `(nil ,list nil))

(defun my-real-zipper/get-left (zipper)
  (first zipper))

(defun my-real-zipper/get-right (zipper)
  (second zipper))

(defun my-real-zipper/get-up (zipper)
  (third zipper))

(defun my-real-zipper/get-down (zipper)
  (my-real-zipper/element zipper))

(defun my-real-zipper/element (zipper)
  "returns current zipper element"
  (let ((right (my-real-zipper/get-right zipper))
        (left (my-real-zipper/get-left zipper)))
    (if right (first right) (first left))))

(defun my-real-zipper/right (zipper)
  "returns zipper shifted to right"
  (let ((right (my-real-zipper/get-right zipper))
        (left (my-real-zipper/get-left zipper))
        (up (my-real-zipper/get-up zipper)))
    (if right
      (if left
        (list (cons (first right) left) (cdr right) up)
        (list (list (first right)) (cdr right) up)) 
      zipper)))

(defun my-real-zipper/left (zipper)
  "returns zipper shifted to left"
  (let ((right (my-real-zipper/get-right zipper))
        (left (my-real-zipper/get-left zipper)))
    (if left
      (if right
        (list (cdr left) (cons (first left) right) (my-real-zipper/get-up zipper))
        (list (cdr left) (list (first left)) (my-real-zipper/get-up zipper)))
      zipper)))

(defun my-real-zipper/up (zipper)
  "returns zipper shifted up"
  (let ((up (my-real-zipper/get-up zipper)))
    (if up
      (list 
        (my-real-zipper/get-left up) 
        (my-real-zipper/get-right up)
        (my-real-zipper/get-up up))
      zipper)))

(defun my-real-zipper/down (zipper)
  "returns zipper shifted down"
  (let ((down (my-real-zipper/get-down zipper)))
    (if (consp down)
      (list 
        (my-real-zipper/get-left (my-real-zipper/from-list down)) 
        (my-real-zipper/get-right (my-real-zipper/from-list down))
        zipper)
      zipper)))


(ert-deftest zipper-from-list ()
  (let ((my-real-zipper (my-real-zipper/from-list '(1 2 3 4))))
    (should (equal my-real-zipper '(nil (1 2 3 4) nil)))))

(ert-deftest zipper-element ()
  (let ((my-real-zipper (my-real-zipper/from-list '(1 2 3 4))))
    (should (equal (my-real-zipper/element my-real-zipper) 1))))

(ert-deftest zipper-element-at-end ()
  (let ((my-real-zipper (my-real-zipper/right (my-real-zipper/from-list '(1)))))
    (should (equal (my-real-zipper/element my-real-zipper) 1))))

(ert-deftest zipper-right ()
  (let ((my-real-zipper (my-real-zipper/from-list '(1 2 3 4))))
    (should (equal (my-real-zipper/right my-real-zipper) '((1) (2 3 4) nil)))))

(ert-deftest zipper-right-at-end ()
  (let ((my-real-zipper (my-real-zipper/from-list '(1))))
    (should (equal (my-real-zipper/right my-real-zipper) '((1) nil nil)))))

(ert-deftest zipper-left ()
  (let ((my-real-zipper (my-real-zipper/right (my-real-zipper/from-list '(1 2)))))
    (should (equal (my-real-zipper/left my-real-zipper) '(nil (1 2) nil)))))

(ert-deftest zipper-left-at-beginning ()
  (let ((my-real-zipper (my-real-zipper/from-list '(1))))
    (should (equal (my-real-zipper/left my-real-zipper) '(nil (1) nil)))))

(ert-deftest zipper-down-up-at-beginning ()
  (let ((my-real-zipper (my-real-zipper/from-list '((1)))))
    (should (equal (my-real-zipper/up (my-real-zipper/down my-real-zipper)) my-real-zipper))))

(ert-deftest zipper-up-at-beginning ()
  (let ((my-real-zipper (my-real-zipper/from-list '((1)))))
    (should (equal (my-real-zipper/up my-real-zipper) my-real-zipper))))

(ert-deftest zipper-down-at-beginning ()
  (let ((my-real-zipper (my-real-zipper/from-list '((1)))))
    (should (equal (my-real-zipper/element (my-real-zipper/down my-real-zipper)) 1))))

(ert-deftest zipper-down ()
  (let ((my-real-zipper (my-real-zipper/from-list '((1 (2))))))
    (should (equal (my-real-zipper/element (my-real-zipper/down (my-real-zipper/right (my-real-zipper/down my-real-zipper)))) 2))))

(ert-deftest zipper-up ()
  (let ((my-real-zipper (my-real-zipper/from-list '((1 (2))))))
    (should (equal (my-real-zipper/element (my-real-zipper/down (my-real-zipper/right (my-real-zipper/down my-real-zipper)))) 2))))

(ert-run-tests-interactively 't)
