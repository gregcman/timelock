;;;; timelock.lisp

(in-package #:timelock)

(defun string-to-unsigned-byte-8-array (str)
  "Converts a string to an array of (unsigned-byte 8)."
  (map ' (vector (unsigned-byte 8)) #'char-code str))

;;https://gwern.net/self-decrypting

;;(struct-to-clos:struct->class) 
(defstruct (chain (:type list))
  (encrypted-start)
  (start)
  (end)
  (length))

(defparameter *this-directory* (utility:this-directory))
(defparameter *save-dir* (merge-pathnames "saves/" *this-directory*))


(defun save-file (name)
  (merge-pathnames name *save-dir*))
(defun new-file ()
  (save-file (format nil "~a.txt" (write-to-string (get-universal-time)))))
(defun save-chain (&optional (chainlist (encrypt-chainlist)) (file (new-file)))
  (alexandria:write-string-into-file (write-to-string chainlist)
                                     file
                                     :if-exists :overwrite
                                     :if-does-not-exist :create))
(defun load-chain (&optional (name (save-file "3921276338.txt")))
  (let ((lispobj (read-from-string (alexandria:read-file-into-string name))))
    (dotimes (i (length lispobj))
      (let ((rawchain (aref lispobj i)))
        (setf (aref lispobj i)
              (make-chain
               :encrypted-start (convertarray (elt rawchain 0))
               :start (convertarray (elt rawchain 1))
               :end (convertarray (elt rawchain 2))
               :length (elt rawchain 3)))))
    lispobj))

(defun convertarray (&optional (arr #(1 3 4 5)))
  (when arr
    (make-array (length arr) :element-type '(unsigned-byte 8) :initial-contents arr)))

(defparameter *chain-length* 100)

(defmethod print-object ((object chain) stream)
  (format stream "(~% ~s ~% ~s ~% ~s ~% ~s)"
	  (chain-encrypted-start object)
          (chain-start object)
	  (chain-end object)
	  (chain-length object)))

;;file -> list of encrypted-chains
;;TODO - make sure everything is checked multiple times.
;;If there is an error then all the data is lost.

(defun seed-chain (&optional (individual-chain-length *chain-length*))
  (make-chain
   :start (make-random-buffer)
   :length individual-chain-length))
(defun compute-end-hash (&optional (chain (seed-chain)))
  (setf (chain-end chain)
        (redigest (chain-length chain)
                  (chain-start chain)))
  chain)

(defun copy-chainlist (chainlist)
  (let* ((len (length chainlist))
         (arr (make-array len)))
    (dotimes (i len)
      (let ((c (aref chainlist i)))
        (setf (aref arr i)
              (make-chain
               :encrypted-start (chain-encrypted-start c)
               :start (chain-start c)
               :end (chain-end c)
               :length (chain-length c)))))
    arr))

(defun seed-chains (&optional (count 17) (individual-chain-length *chain-length*))
  (let ((acc))
    (dotimes (i count)
      (push (seed-chain individual-chain-length) acc))
    (make-array (length acc) :initial-contents acc)))
(defun make-chains (&optional (chainlist (seed-chains)))
  (let ((len (length chainlist)))
    (dotimes (i len)
      (compute-end-hash (aref chainlist i))))
  chainlist)

(defun encrypt-chainlist (&optional (chainlist (make-chains)))
  (let ((len (length chainlist)))
    (dotimes (i (1- len))
      (let ((next (aref chainlist (1+ i))))
        (setf (chain-encrypted-start next)
              (de-en-crypt
               (chain-start next)
               (chain-end (aref chainlist i)))))))
  chainlist)

(defun de-en-crypt-payload (&optional
                          (payload (make-random-buffer))
                          (chainlist (make-chains)))
  (de-en-crypt payload
               (chain-end (aref chainlist (1- (length chainlist))))))

(defun obscure-chainlist (&optional (chainlist (encrypt-chainlist)))
  (let ((l (length chainlist)))
    (dotimes (i (1- l))
      (let ((next (aref chainlist (1+ i))))
        (setf (chain-start next) nil)))
    (dotimes (i l)
      (let ((next (aref chainlist i)))
        (setf (chain-end next) nil))))
  chainlist)

(defun decrypt-chainlist (&optional (chainlist (obscure-chainlist)) (index (1- (length chainlist))))
  (block nil
    (let* ((chain (aref chainlist index)))
      (symbol-macrolet ((start (chain-start chain))
                        (end (chain-end chain)))
        (when end
          (return end))
        (when (not start)
          (setf start
                (de-en-crypt
                 (decrypt-chainlist chainlist (1- index))
                 (chain-encrypted-start chain))))
        (return (setf end (redigest (chain-length chain) start)))))))

(defun test ()
  (let* ((payload (make-random-buffer))
         (a (seed-chains 32 10000))
         (b (time (make-chains a)))
         (encrypted-payload (de-en-crypt-payload payload b))
         (c (encrypt-chainlist b))
         (d (obscure-chainlist (copy-chainlist c)))
         (__ (progn
                                        ;(print c)
                                        ;(print d)
               ))
         (e (time (decrypt-chainlist d)))
         (_ ;(print e)
           )
         (decrypted-payload (de-en-crypt encrypted-payload e)))
    (declare (ignore __ _))
    (equalp payload decrypted-payload)))

(defun test-file ()
  (save-file "test.lisp"))
(defun test-with-file ()
  (let* ((payload
           (make-random-buffer)
           )
         (a (seed-chains 32 10000))
         (b (time (make-chains a)))
         (encrypted-payload (de-en-crypt-payload payload b))
         (c (encrypt-chainlist b))
         (d (obscure-chainlist (copy-chainlist c)))
         (file (test-file))
         (__ (progn
               (save-chain d file)
               ;;(print c)
               ;;(print d)
               ))
         (loaded (load-chain file))
         (___ (progn
                (print (equalp d loaded))
                ;;(print d)
                ;;(print loaded)
                ))
         (e (time (decrypt-chainlist loaded)))
         (_ ;(print e)
           )
         (decrypted-payload (de-en-crypt encrypted-payload e)))
    (declare (ignore __ _ ___))
    (print (equalp payload decrypted-payload))))


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
      array)))


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
