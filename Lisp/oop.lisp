;; Refolli Francesco 865955

;;; hash-table interface

(defun hash-table-init ()
    (make-hash-table))

(defun hash-table-get (ht key &optional (default nil))
    (assert (hash-table-p ht))
    (gethash key ht default))

(defun hash-table-set (ht key datum)
    (assert (hash-table-p ht))
    (setf (gethash key ht) datum)
    ht)

(defun hash-table-exists (ht key)
    (assert (hash-table-p ht))
    (nth-value 1 (gethash key ht)))

(defun hash-table-keys (ht)
    (let ((k-list (list)))
        (maphash
            #'(lambda (k v)
                (setf
                    k-list
                    (append k-list (list k))))
            ht)
        k-list))

(defun hash-table-copy (ht)
  (let ((new-table (make-hash-table
                    :test (hash-table-test ht)
                    :size (hash-table-size ht))))
    (maphash #'(lambda(key value)
                 (setf (gethash key new-table) value))
             ht)
    new-table))

;;; class db

;;;;
;   
;   I keep a hash-table of all class definitions
;   
;   INIT : class-db-init
;   GET  : class-db-get
;   SET  : class-db-set
;   
;;;;

(defun class-db-init ()
    (defparameter *class-db* (hash-table-init)))

(defun class-db-set (name class)
    (hash-table-set *class-db* name class))

(defun class-db-get (name)
    (hash-table-get *class-db* name))

(defun class-db-exists (name)
    (not (null (class-db-get name))))

;;; method db

;;;;
;   
;   I keep a hash-table of all method handler definitions
;   so that i don't need to redefine handlers again
;   
;   INIT : method-db-init
;   GET  : method-db-get
;   SET  : method-db-set
;   
;;;;

(defun method-db-init ()
    (defparameter *method-db* (hash-table-init)))

(defun method-db-set (name method)
    (hash-table-set *method-db* name method))

(defun method-db-get (name)
    (hash-table-get *method-db* name))

(defun method-db-exists (name)
    (not (null (method-db-get name))))

(defun method-db-gen (name)
    (let (
        (functor 
            (lambda (object &rest args)
                (apply
                    (coerce (<< object name) 'function)
                    (cons object args)))))
        (setf
            (fdefinition name)
            functor)
        functor))

(defun method-db-assert (name)
    (if
        (not (method-db-exists name))
        (method-db-set name
            (method-db-gen name))))

(defun is-method (value)
    (and
        (not (null value))
        (not (atom value))
        (equal (first value) '=>)))

(defun method-rewrite (method)
    (append
        (list
            'lambda
            (cons 'this (second method)))
        (rest (rest method))))

;;; class definition

(defun class-def-assemble (name parents slots)
    (list 'class name parents slots))

(defun is-class-def (value)
    (and
        (not (null value))
        (not (atom value))
        (equal (first value) 'class)))

(defun class-def-name (value)
    (assert (is-class-def value))
    (second value))

(defun class-def-parents (value)
    (assert (is-class-def value))
    (third value))

(defun class-def-slots (value)
    (assert (is-class-def value))
    (fourth value))

(defun class-resolve-parent (parent)
    (assert (class-db-exists parent))
    (cons
        parent
        (class-def-parents
            (class-db-get parent))))

(defun class-resolve-parents (parents)
    (if (null parents) nil
        (remove-duplicates
            (append
                (class-resolve-parent (first parents))
                (class-resolve-parents (rest parents))))))

(defun class-def-stub (name parents)
    (class-def-assemble
        name
        (class-resolve-parents parents)
        (hash-table-init)))

;;; class decoration

(defun class-dec-parent-slot (class parent-slots slot-key)
    (let (
            (name (class-def-name class))
            (parents (class-def-parents class))
            (slots (class-def-slots class)))
        (class-def-assemble
            name
            parents
            (hash-table-set
                slots slot-key
                (hash-table-get parent-slots slot-key)))))

(defun class-dec-parent-slots (class slots slot-keys)
    (if (null slot-keys) class
        (class-dec-parent-slots
            (class-dec-parent-slot class slots (car slot-keys))
            slots
            (cdr slot-keys))))

(defun class-dec-parent (class parent)
    (let ((parent-slots
            (class-def-slots
            (class-db-get parent))))
        (let ((slot-keys
                (hash-table-keys parent-slots)))
            (class-dec-parent-slots
                class parent-slots slot-keys))))

(defun class-dec-parents (class parents)
    (if (null parents) class
        (class-dec-parents
            (class-dec-parent class (car parents))
            (cdr parents))))

(defun class-dec-slot-value-direct (class slot-name slot-value)
    (let (
            (name (class-def-name class))
            (parents (class-def-parents class))
            (slots (class-def-slots class)))
        (class-def-assemble
            name
            parents
            (hash-table-set
                slots slot-name
                slot-value))))

(defun class-dec-slot-field (class slot-name slot-value)
    (class-dec-slot-value-direct class slot-name slot-value))

(defun class-dec-slot-method (class slot-name slot-value)
    (method-db-assert slot-name)
    (let ((rewritten (method-rewrite slot-value)))
        (class-dec-slot-value-direct
            class slot-name rewritten)))

(defun class-dec-slot-value (class slot-name slot-value)
    (if (is-method slot-value)
        (class-dec-slot-method class slot-name slot-value)
        (class-dec-slot-field class slot-name slot-value)))

(defun class-dec-slot-values (class slot-values)
    (if (null slot-values) class
        (let (
            (slot-name (first slot-values))
            (slot-value (second slot-values)))
            (class-dec-slot-values
                (class-dec-slot-value class slot-name slot-value)
                (rest (rest slot-values))))))

(defun class-dec (class parents slot-values)
    (class-dec-slot-values
        (class-dec-parents
            class parents)
        slot-values))

;;; object db

;;;;
;   
;   I keep a hash-table of all object definitions
;   so that i don't need to remember object symbols
;   
;   INIT : object-db-init
;   GET  : object-db-get
;   SET  : object-db-set
;   
;;;;

(defun object-db-init ()
    (defparameter *object-db* (hash-table-init)))

(defun object-db-set (name object)
    (hash-table-set *object-db* name object))

(defun object-db-get (name)
    (hash-table-get *object-db* name))

(defun object-class (object)
    (second object))

(defun object-parents (object)
    (third object))

(defun object-slots (object)
    (fourth object))

(defun object-stub (class-name)
    (let ((class (class-db-get class-name)))
        (list
            'object
            class-name
            (class-def-parents class)
            (hash-table-copy (class-def-slots class)))))

(defun object-has-slot (object slot-name)
    (let ((slots (object-slots object)))
        (hash-table-exists slots slot-name)))

(defun object-get-slot (object slot-name)
    (let ((slots (object-slots object)))
        (hash-table-get slots slot-name)))

(defun object-update-slot (object slot-name slot-value)
    (assert (object-has-slot object slot-name))
    (let ((slots (object-slots object)))
        (list
            'object
            (object-class object)
            (object-parents object)
            (hash-table-set
                (object-slots object)
                slot-name slot-value))))

(defun object-update-slots (object slot-values)
    (if (null slot-values) object
        (let (
            (slot-name (first slot-values))
            (slot-value (second slot-values)))
            (object-update-slots
                (object-update-slot object slot-name slot-value)
                (rest (rest slot-values))))))

(defun object-is-instance (object class-name)
    (if (equal class-name t) t
        (member class-name
            (cons
                (object-class object)
                (object-parents object)))))

(defun is-object (value)
    (and
        (not (null value))
        (not (atom value))
        (equal (first value) 'object)))

;;; antoniotti interface

(defun def-class (name parents &rest slot-values)
    (let 
        ((class-stub (class-def-stub name parents)))
        (let 
            ((class-value
                (class-dec class-stub parents slot-values)))
            (class-db-set name class-value))))

(defun create (class-name &rest slot-values)
    (assert (class-db-exists class-name))
    (let ((object-stub (object-stub class-name)))
        (object-update-slots object-stub slot-values)))

(defun is-class (class-name)
    (class-db-exists class-name))

(defun is-instance (value class-name)
    (and
        (is-object value)
        (object-is-instance value class-name)))

(defun << (value slot-name)
    (assert (is-object value))
    (assert (object-has-slot value slot-name))
    (object-get-slot value slot-name))

(defun <<* (value slot-name &rest slot-names)
    (if (null slot-names)
        (<< value slot-name)
        (apply #'<<*
            (append
                (list
                    (<< value slot-name)
                    (car slot-names))
                (cdr slot-names)))))

;;; init

(class-db-init)
(method-db-init)
(object-db-init)

;;; test

(defun test-def-class ()
    (def-class 'ClassA '() '0 0 '1 1)
    (def-class 'ClassB '() '2 2 '3 3 'hello '(=> (x) (+ x 42)))
    (def-class 'ClassC '(ClassA ClassB) '4 4 '5 5 'a nil'b nil))

(defun test-is-class ()
    (assert (is-class 'ClassA))
    (assert (is-class 'ClassB))
    (assert (is-class 'ClassC))
    (assert (not (is-class 'ClassD))))

(defun test-create ()
    (defparameter ObjectA (create 'ClassA))
    (object-db-set 'ObjectA ObjectA)
    (defparameter ObjectB (create 'ClassB))
    (object-db-set 'ObjectB ObjectB)
    (defparameter ObjectC (create 'ClassC 'a ObjectA 'b ObjectB))
    (object-db-set 'ObjectC ObjectC))

(defun test-is-instance ()
    (assert (is-instance ObjectA 'ClassA))
    (assert (is-instance ObjectB 'ClassB))
    (assert (is-instance ObjectC 'ClassC))
    (assert (is-instance ObjectC 'ClassA))
    (assert (is-instance ObjectC 'ClassB))
    (assert (not (is-instance ObjectC 'ClassD))))

(defun test-<< ()
    (assert (equal (<< ObjectA '0) 0))
    (assert (is-instance (<< ObjectC 'a) 'ClassA))
    (assert (is-instance (<< ObjectC 'b) 'ClassB)))

(defun test-<<* ()
    (assert (equal (<<* ObjectC 'a '0) 0)))

(defun test-method ()
    (assert (equal (hello ObjectB 1) 43))
    (assert (equal (hello ObjectC 2) 44)))

(defun test ()
    (test-def-class)
    (test-is-class)
    (test-create)
    (test-is-instance)
    (test-<<)
    (test-<<*)
   (test-method)
)

;;; OMNI-PRINT

(defun print-classes ()
    (print "Classes:")
    (print *class-db*))

(defun print-methods ()
    (print "Methods:")
    (print *method-db*))

(defun print-objects ()
    (print "Objects:")
    (print *object-db*))

;;; RUNTIME

(test)

(print-classes)
(print-methods)
(print-objects)