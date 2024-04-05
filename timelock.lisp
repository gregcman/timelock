;;;; timelock.lisp

(in-package #:timelock)

(defun string-to-unsigned-byte-8-array (str)
  "Converts a string to an array of (unsigned-byte 8)."
  (map ' (vector (unsigned-byte 8)) #'char-code str))

;;https://gwern.net/self-decrypting

(progn;;utility:with-unsafe-speed
  (defun redigest (&optional (string "foo") (n 100))
    (let ((buffer (string-to-unsigned-byte-8-array string))
          (array (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
      (dotimes (i n)
        (ironclad:digest-sequence :sha256 buffer :digest array)
        (setf buffer array)
        (print array)))))


(defun main ()
  (let ((tasks ()))
    (sucle-mp::with-initialize-multiprocessing
      (dotimes (n 100)
        (let ((task (sucle-mp::submit (lambda () (redigest)))))
          (push task tasks))))
    (block loop-exit
      (loop
        (block out
          (dolist (task tasks)
            (when (eq nil (sucle-mp::job-task-finished task))
              (return-from out))))
        (return-from loop-exit)))))
