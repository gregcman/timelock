;;;; timelock.lisp

(in-package #:timelock)

(defun string-to-unsigned-byte-8-array (str)
  "Converts a string to an array of (unsigned-byte 8)."
  (map ' (vector (unsigned-byte 8)) #'char-code str))

;;https://gwern.net/self-decrypting

(struct-to-clos:struct->class 
    (defstruct chain
      (encrypted-start)
      (start)
      (end)
      (length)))

(defparameter *chain-length* 100)

(defmethod print-object ((object chain) stream)
  (format stream "<chain ~% ~s ~% ~s ~% ~s ~% ~s>"
	  (chain-encrypted-start object)
          (chain-start object)
	  (chain-end object)
	  (chain-length object)))

;;file -> list of encrypted-chains

(defun make-chains (&optional (count 17) (individual-chain-length *chain-length*))
  (let ((acc))
    (dotimes (i count)
      (push
       (redigest individual-chain-length)
       acc))
    (make-array (length acc) :initial-contents acc)))

(defun encrypt-chainlist (&optional (chainlist (make-chains)))
  (let ((len (length chainlist)))
    (dotimes (i (1- len))
      (let ((next (aref chainlist (1+ i))))
        (setf (chain-encrypted-start next)
              (de-en-crypt
               (chain-start next)
               (chain-end (aref chainlist i)))))))
  chainlist)

(defun decrypt-chainlist)

;;since the key is pseudorandom and same as the length of the plaintext just XOR it
(defun de-en-crypt (data key &optional (buffer (make-buffer)))
  (dotimes (i 32)
    (setf (aref buffer i)
          (logxor (aref data i)
                  (aref key i))))
  buffer)

(defun test-de-en-crypt ()
  (let* ((key (make-random-buffer))
         (data (make-random-buffer))
         (encrypted (de-en-crypt data key))
         (decrypted (de-en-crypt encrypted key)))
    (terpri)
    (print key)
    (terpri)
    (print data)
    (terpri)
    (print encrypted)
    (terpri)
    (print decrypted)))

(declaim (inline make-buffer))
(defun make-buffer ()
  (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
(defun make-random-buffer ()
  (crypto:random-data 32))

(defun make-dummy-chain (&optional (array (make-buffer)))
  (make-chain
   :end array))


(utility:with-unsafe-speed
  (defun redigest (&optional (n 100) (data (make-random-buffer)))
    (declare (type fixnum n))
    (when (not (plusp n))
      (error "bad amount of hashes"))
    (let ((buffer data)
          (array (make-buffer)))
      (dotimes (i n)
        (ironclad:digest-sequence :sha256 buffer :digest array)
        (setf buffer array))
      (make-chain
       :start data
       :end array
       :length n))))


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
