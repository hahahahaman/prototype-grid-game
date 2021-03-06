;;;; matrix-transforms.lisp

(in-package #:matrix-transforms)

;;; "matrix-transforms" goes here. Hacks and glory await!

(defvar *global-setfs* nil)

(defmacro defglobal (var &optional (val nil valp) (doc nil docp))
  "Creates a global variable and adds a lambda to *GLOBAL-SETFS* that
can be used to reset VAR to VAL."
  (append `(progn)
          (if (and valp docp)
              `((defvar ,var ,val ,doc))
              `((defvar ,var ,val)))
          `((setf (getf *global-setfs* (intern (string ',var) :keyword))
                  (lambda () (setf ,var ,val))))))

;;; modes

;; debug mode, default nil
(defglobal *debug* nil)
;; ;; paused
;; (defglobal *paused* nil)

;;; screen
(defglobal *width* 800)
(defglobal *height* 800)

;;; delta time
;; *DT* keeps track of the time since last frame, in seconds
;; *PREVIOUS-TIME* gives the time, in seconds, of the previous frame
;; since the start of the program, and (glfw:get-time) returns the
;; current time
(defconstant +max-fps+ 150)
(defglobal *dt* 0.02d0)
(defglobal *previous-time* 0.0)

;;; time travel
;; by using the fset library for shared-structure immutable data structures
;; program can keep track of state at each frame and then move around the
;; this total frame state
(defenum:defenum *enum-time-travel-state* ((+time-play+ 0)
                                           +time-paused+
                                           +time-rewind+
                                           +time-forward+))

(defglobal *time-travel-state* +time-play+)
(defglobal *current-frame* 0)
(defglobal *max-frame-index* 0)
(defglobal *timeline*
    (make-array 500000 :element-type 'list
                       :initial-element nil
                       :adjustable t
                       :fill-pointer 0))

(defglobal *tracked-vars* nil)

;;; actions
;; p-lists that keep track of the current actions on keys and buttons
(defglobal *key-actions* ())
(defglobal *mouse-button-actions* ())
(defglobal *key-pressed* ())
(defglobal *mouse-button-pressed* ())

;;; singletons
;; singletons used for resource management and rendering
(defglobal *program-manager* nil)
(defglobal *texture-manager* nil)
(defglobal *sprite-renderer* nil)
(defglobal *rect-renderer* nil)

;;; cursor position values
(defglobal *cursor-callback-p* nil) ;; cursor has been moved
(defglobal *first-mouse* t) ;; checks if first time cursor has been moved

;; current cursor position
(defglobal *cursor-x* (/ *width* 2.0))
(defglobal *cursor-y* (/ *height* 2.0))

;; previous cursor position
(defglobal *last-x* (/ *width* 2.0))
(defglobal *last-y* (/ *height* 2.0))

;; the scroll wheel has been used
(defglobal *scroll-callback-p* nil)

;; number of ticks of the scroll wheel
(defglobal *scroll-x* (/ *width* 2.0))
(defglobal *scroll-y* (/ *height* 2.0))

;;; entities
(defglobal *entities* (empty-map))
(defglobal *destructive-changes* ())

;;; utils
(let* ((max-samples 500)
       (samples (make-array max-samples
                            :element-type 'double-float
                            :initial-element (/ 1.0d0 +max-fps+)))
       (sample-index 0))
  (defun average-fps ()
    (setf (aref samples sample-index) *dt*
          sample-index (mod (1+ sample-index) max-samples))
    ;; (print *dt*)
    (/ max-samples (reduce #'+ samples))))

;; global specific utils
(defun clear-actions ()
  "Clears the input actions from last frame."
  (setf *key-actions* nil
        *mouse-button-actions* nil))

(defun update-dt ()
  "Called in the main loop, updates time globals."
  (setf *dt* (- (glfw:get-time) *previous-time*)
        *previous-time* (glfw:get-time))

  ;; prevent unruly time steps from breaking game
  ;; keep time step between 0 and 0.25
  (setf *dt* (max 0.0d0 (min 0.25d0 *dt*))))

(defun cap-fps ()
  "Cap frame rate, preventing resource hogging."
  (let ((frame-diff (- (+ (/ 1.0d0 +max-fps+) *previous-time*) (glfw:get-time))))
    (when (> frame-diff 0)
      (sleep frame-diff))))

(defun update-globals ()
  "A single function that encompasses global updates."
  (clear-actions)
  (update-dt)
  (cap-fps))

(defun update-window-title (window title)
  (cl-glfw3:set-window-title
   (format nil "~A | fps: ~A | time: ~A | time-travel-state: ~A"
           title
           (round (average-fps))
           *current-frame*
           (cond ((eql *time-travel-state* +time-play+)
                  "PLAY")
                 ((eql *time-travel-state* +time-paused+)
                  "PAUSED")
                 ((eql *time-travel-state* +time-forward+)
                  (format nil "FORWARD x~d"
                          (aref *time-speed-multiplier* *time-speed-index*)))
                 ((eql *time-travel-state* +time-rewind+)
                  (format nil "REWIND x~d"
                          (aref *time-speed-multiplier* *time-speed-index*)))))
   window))

(defun initialize-globals ()
  "Sets all defined globals to their VAL."
  (iter (for (var-symbol func) on *global-setfs* by #'cddr)
    (funcall func)))

(defun key-action-p (key action)
  "Returns true if KEY is in *key-actions* and its state is EQ to ACTION."
  (let ((state (getf *key-actions* key)))
    (and (not (null state)) ;; if STATE is not null then key must have been found
         (eq action state))))

(defun key-pressed-p (key)
  "RETURNS true if key is currently pressed."
  (getf *key-pressed* key))

(defun mouse-button-action-p (button action)
  "Returns true if KEY is in *mouse-button-actions* and its state is EQ to ACTION."
  (let ((state (getf *mouse-button-actions* button)))
    (and (not (null state)) ;; if STATE is not null then button be active
         (eq action state))))
(defun mouse-button-pressed-p (button)
  (getf *mouse-button-pressed* button))

;;; type utils
(defun concat-vecs (&rest vecs)
  "Creates of single-float simple-array from lone values, lists, and/or vectors."
  (let* ((len 0) ;; keeps track of simple-array length
         (vec (apply #'concatenate ;; combine all sequences, lists and vectors
                     'vector ;; output a vector

                     ;; ensure all of the elements of VECS are of type sequence
                     (mapcar
                      (lambda (x)
                        (cond ((typep x 'sequence)
                               (incf len (length x))
                               x) ;; keep track of length
                              (t
                               (incf len 1)
                               (list (coerce x 'single-float)))))
                      vecs))))
    ;; finally output simple-array
    (coerce vec `(simple-array single-float (,len)))))

(defun sequence-to-gl-array (sequence type)
  "Creates a gl-array from a lisp sequence, with elements of type TYPE.
Remember to free gl-array afterwards."
  (let ((gl-arr (gl:alloc-gl-array type (length sequence))))
    (dotimes (i (length sequence)
                gl-arr)
      (setf (gl:glaref gl-arr i) (elt sequence i)))))

(defmacro with-sequence-to-gl-array ((var sequence type) &body body)
  `(let ((,var (sequence-to-gl-array ,sequence ,type)))
     ,@body
     (gl:free-gl-array ,var)))

(defun square (x)
  (* x x))
(defun cube (x)
  (* x x x))

(defun dist-mod (a b modulo)
  "Distance between A and B, when mod MODULO."
  (min (mod (- a b) modulo) (mod (- b a) modulo)))

(defun random-in-range (start end)
  "Random number between start and end inclusive."
  (+ start (random (- (1+ end) start))))

(declaim (ftype (function (real) single-float) cfloat))
(defun cfloat (n)
  (declare (optimize (speed 3) (safety 0)))
  "Coerce N to single-float. Just makes the function shorter."
  (coerce n 'single-float))

(defun sizeof (type)
  "Gives to foreign-type-size of TYPE. Used with cffi stuff, like opengl."
  (cffi-sys:%foreign-type-size type))

(defun sizeof* (type multiple)
  "Multiply sizeof TYPE, by MULTIPLE"
  (* (sizeof type) multiple))

(defmacro define-vec-op (name func &rest args)
  `(defun ,name (,@args)
     (cl:map (type-of ,(car args))
             ,func
             ,@args)))

(defun vec-add (v1 v2)
  ;; (declare (optimize (speed 3) (safety 0)))
  "Returns a vector of the same type as V1, which is a component-wise sum of
V1 and V2."
  (cl:map (type-of v1) #'+ v1 v2))

(defun vec-mul (v1 f)
  "Returns a vector with the same type as V1, which has components multiplied by F."
  (cl:map (type-of v1)
          (lambda (x) (* (the single-float x) (the single-float f)))
          v1))

(defun vec-div (v1 f)
  (vec-mul v1 (/ 1.0 f)))

(defun vec-length (v)
  (sqrt (reduce #'+ (cl:map (type-of v) #'square v))))

(defun clamp (value low high)
  (declare (optimize (speed 3) (safety 0)))
  (min high (max low value)))

(declaim (ftype (function (single-float single-float single-float) single-float) sfclamp))
(defun sfclamp (value low high)
  (declare (optimize (speed 3) (safety 0)))
  (min high (max low value)))

(defun vec-clamp (value low high)
  (cl:map (type-of value) #'sfclamp value low high))

(declaim (ftype (function (vec2 vec2) vec2) vec2-add))
(defun vec2-add (v1 v2)
  (declare (optimize (speed 3) (safety 0)))
  (cl:map 'vec2 #'+ v1 v2))

(declaim (ftype (function (vec2 single-float) vec2) vec2-mul))
(defun vec2-mul (v1 f)
  (declare (optimize (speed 3) (safety 0)))
  (cl:map 'vec2 (lambda (x) (* x f)) v1))

(declaim (ftype (function (vec2 single-float) vec2) vec2-div))
(defun vec2-div (v1 f)
  (vec2-mul v1 (/ 1.0 f)))

(declaim (ftype (function (vec2 vec2) vec2) vec2-add))
(defun vec2-sub (v1 v2)
  (declare (optimize (speed 3) (safety 0)))
  (cl:map 'vec2 #'+ (the vec2 v1) (vec2-mul v2 -1.0)))

(declaim (ftype (function (vec2 vec2 vec2) vec2) vec2-clamp))
(defun vec2-clamp (value low high)
  (declare (optimize (speed 3) (safety 0)))
  (cl:map 'vec2 #'sfclamp value low high))

(defun x-val (vec)
  (aref vec 0))
(defun y-val (vec)
  (aref vec 1))
(defun z-val (vec)
  (aref vec 2))
(defun w-val (vec)
  (aref vec 3))

;; trying to cut back on destructive changes
;; (defun (setf x-val) (value vec)
;;   (setf (aref vec 0) value))
;; (defun (setf y-val) (value vec)
;;   (setf (aref vec 1) value))
;; (defun (setf z-val) (value vec)
;;   (setf (aref vec 2) value))

(defmacro get-slot (object &rest nested-slot-names)
  "Expands into an expression:
(slot-value (slot-value ... (slot-value object 'name_1) ... 'name_n-1) 'name_n)
Useful for setting a slot of an object that is a slot in another object."
  (iter (iter:with current = object)
    (for s in nested-slot-names)
    (setf current `(slot-value ,current ,s))
    (finally (return current))))

;;; file io

(defun read-sexp-from-file (filename)
  "Reads all sexp from file FILENAME, returning a list of all collected."
  (with-open-file (file filename :direction :input)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (iter (for sexp = (read file nil))
          (while sexp)
          (collect sexp))))))

(defun read-entire-file (filename)
  "DEPRECATED. Returns a string with the entire content of a FILENAME, including
whitespaces."
  (with-open-file (file filename :direction :input)
    (if file
        (let ((str ""))
          (iter
            (for line = (read-line file nil nil))
            (while line)
            (setf str (concatenate 'string str (format nil "~a~%" line))))
          str)
        (error "Unable to open file ~a.~%" file))))

;; swank stuff
(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
  error. Remember to hit C in slime or pick the restart so errors don't kill the app."
  `(restart-case
       (progn ,@body) (continue () :report "Continue")))

(defun update-swank ()
  (ignore-errors
   "Called from within the main loop, this keep the lisp repl running"
   (continuable
    (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t))))))

;; copy instances

(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name
                                 (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

;; Threading macro from clojure
(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
      (if more
          `(-> (-> ,x ,form) ,@more)
          (if (listp form)
              `(,(car form) ,x ,@(cdr form))
              (list form x)))
      x))

(defun drop-nth (n list)
  (append (subseq list 0 n) (nthcdr (1+ n) list)))

(defun add-nth (n elem list)
  (append (subseq list 0 n) (cons elem (nthcdr n list))))

(defun set-nth (n elem list)
  (append (subseq list 0 n) (cons elem (nthcdr (1+ n) list))))

(defun plist-set (place indicator value)
  (do ((plist place (cddr plist))
       (n 0 (+ 2 n)))
      ((null plist) (append place (list indicator value)))
    (cond ((atom (cdr plist))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (cdr plist)
                  :expected-type 'cons))
          ((eq (car plist) indicator)
           (return (append (subseq place 0 (1+ n))
                           (cons value (nthcdr (+ n 2) place))))))))

(defun get-map-keys (to-type map)
  (image (lambda (x) (car x)) (convert to-type map)))
(defun get-map-values (to-type map)
  (image (lambda (x) (cdr x)) (convert to-type map)))

;;; input
;; keys pressed
(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (setf (getf *key-actions* key) action)

  ;; trouble getting :repeat events, so keep key pressed until :release
  (cond ((eq action :press)
         (setf (getf *key-pressed* key) t))
        ((eq action :release)
         (setf (getf *key-pressed* key) nil))))

;; mouse button pressed
(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore window mod-keys))
  (setf (getf *mouse-button-actions* button) action)

  (cond ((eq action :press)
         (setf (getf *mouse-button-pressed* button) t))
        ((eq action :release)
         (setf (getf *mouse-button-pressed* button) nil))))

;; cursor movement
(glfw:def-cursor-pos-callback cursor-callback (window x y)
  (declare (ignore window))
  (cond
    ;; first time cursor moved, initialize *last-x* and *last-y*
    (*first-mouse*
     (setf *last-x* x
           *last-y* y
           *first-mouse* nil))
    ;; set current cursor position
    (t
     (setf *cursor-callback-p* t
           *cursor-x* x
           *cursor-y* y))))

;; scroll wheel
(glfw:def-scroll-callback scroll-callback (window x y)
  (declare (ignore window))
  ;; set scroll wheel movement
  (setf *scroll-callback-p* t
        *scroll-x* x
        *scroll-y* y))

;;; timeline

(defmacro var-keyword (var)
  `(intern (string ',var) :keyword))
(defmacro var-keyword-macro (var)
  ``(intern (string ',,var) :keyword))

(defmacro track-var (var)
  "Adds a var to *TRACKED-VARS*, with a keyword symbol of VAR as the key and a lambda
that returns a list that has stuff that can update *TIMELINE*."
  `(let ((keyword (var-keyword ,var)))
     (setf (getf *tracked-vars* keyword)
           (lambda ()
             (list
              ;; keyword ,var
              :setter
              (let ((var-value ,var))
                (lambda () (setf ,var var-value))))))))

(defmacro track-vars (&rest vars)
  (let ((expr '(progn)))
    (iter (for v in vars)
      (setf expr (append expr `((track-var ,v)))))
    expr))

(defmacro untrack-vars (&rest vars)
  `(setf *tracked-vars*
         (remove-from-plistf *tracked-vars*
                             ,@(mapcar
                                (lambda (v) (var-keyword-macro v))
                                vars))))

(defun update-timeline ()
  "Add plist of tracked values to the current frame"
  (incf *current-frame*)
  (incf *max-frame-index*)
  (vector-push-extend
   (iter (for (var-keyword func) on *tracked-vars* by #'cddr)
     (collect (funcall func)))
   *timeline*))

(defun goto-frame (n)
  "Changes *CURRENT-FRAME* to N, thereby going to the state of frame N."
  ;; constrain between 0 and *MAX-FRAME-INDEX*
  (setf *current-frame* n)
  (when (< *current-frame* 0)
    (pause-pressed)
    (setf *current-frame* 0))
  (when (> *current-frame* *max-frame-index*)
    (pause-pressed)
    (setf *current-frame* *max-frame-index*))

  ;; go through tracked-vars list of that frame setting all values
  (mapcar (lambda (tracked-var) (funcall (getf tracked-var :setter)))
          (aref *timeline* *current-frame*)))

(defun pause-pressed ()
  (when (not (eql *time-travel-state* +time-paused+))
    (setf *time-travel-state* +time-paused+)))

(defun play-pressed ()
  (when (not (eql *time-travel-state* +time-play+))
    ;; erase the future
    (iter (for i from *current-frame* to *max-frame-index*)
      (setf (aref *timeline* i) nil))
    (setf *time-travel-state* +time-play+
          (fill-pointer *timeline*) *current-frame*
          *max-frame-index* *current-frame*)))

;;; rewind and fast-forward
(defglobal *time-speed-multiplier* (vector 1 2 4 8 16 32))
(defglobal *time-speed-index* 0)

(defun forward-pressed ()
  (cond ((not (eql *time-travel-state* +time-forward+))
         (setf *time-travel-state* +time-forward+
               *time-speed-index* 0))
        (t
         (setf *time-speed-index* (mod (1+ *time-speed-index*)
                                       (length *time-speed-multiplier*))))))

(let ((timestep (/ 1.0 60.0d0))
      (accum 0.0d0))
  (defun forward-time ()
    (incf accum *dt*)
    (iter (while (>= accum timestep))
      (goto-frame (+ *current-frame*
                     (aref *time-speed-multiplier* *time-speed-index*)))
      (decf accum timestep))))

(defun rewind-pressed ()
  (cond ((not (eql *time-travel-state* +time-rewind+))
         (setf *time-travel-state* +time-rewind+
               *time-speed-index* 0))
        (t
         (setf *time-speed-index* (mod (1+ *time-speed-index*)
                                       (length *time-speed-multiplier*))))))

(let ((timestep (/ 1.0d0 60.0d0))
      (accum 0.0d0))
  (defun rewind-time ()
    (incf accum *dt*)
    (iter (while (>= accum timestep))
          (goto-frame (- *current-frame*
                         (aref *time-speed-multiplier* *time-speed-index*)))
          (decf accum timestep))))

;;; gl program

(defclass program ()
  ((id
    :type %gl:uint
    :accessor id
    :initarg :id))
  (:default-initargs
   :id 0))

(defmethod initialize-instance ((program program) &key)
  (setf (id program) (gl:create-program))
  t)

(defmethod use ((program program))
  (gl:use-program (id program)))

(defun load-shader-file (filepath shader-type)
  "Creates a compiled shader object of SHADER-TYPE using the file FILEPATH, if
the shader did not compile an error is called."
  (let ((shader (gl:create-shader shader-type))
        (code (alexandria:read-file-into-string filepath))
        (status (cffi:foreign-alloc :int)))
    (gl:shader-source shader code)
    (gl:compile-shader shader)

    (%gl:get-shader-iv shader :compile-status status)
    ;; (format t "compile-status: ~a~%"(cffi:mem-ref status :int))

    ;; 1 means success
    (if (not (= (cffi:mem-ref status :int) 1))
        (progn
          (cffi:foreign-free status)
          (print (gl:get-shader-info-log shader))

          (gl:delete-shader shader)
          (setf shader 0)

          (error "Shader compile-time error, \"~a\"~% error code: ~a~%" filepath
                 (cffi:mem-ref status :int)))
        (progn
          (cffi:foreign-free status)
          shader))))

(defun program-compile (program vert-path frag-path &optional (geo-path nil))
  "Does not return any useful value, calls error is linking of program failed."
  (with-slots (id) program
    (let ((vert (load-shader-file vert-path :vertex-shader))
          (frag (load-shader-file frag-path :fragment-shader))
          (geo (if geo-path
                   (load-shader-file geo-path :geometry-shader)
                   nil)))

      (gl:attach-shader id vert)
      (gl:attach-shader id frag)
      (if geo (gl:attach-shader id geo))

      (gl:link-program id)

      ;; no longer need shaders after link
      (gl:detach-shader id vert)
      (gl:detach-shader id frag)
      (if geo (gl:detach-shader id geo))

      ;; free up shaders so no leak
      (gl:delete-shader vert)
      (gl:delete-shader frag)
      (if geo (gl:delete-shader geo))

      ;; error handling for program
      (let ((status (cffi:foreign-alloc :int)))
        (%gl:get-program-iv id :link-status status)
        ;; (format t "link-status: ~a~%"(cffi:mem-ref status :int))

        (when (not (= (cffi:mem-ref status :int) 1))
          (cffi:foreign-free status)

          (print (gl:get-program-info-log id))
          ;;program no longer needed
          ;;detaches all shaders
          ;;(gl:delete-program id)

          (error "Program link-time error: ~a~%" (gl:get-program-info-log id)))

        (cffi:foreign-free status)))))

(defun get-attrib (program name)
  "Return attrib-location NAME from PROGRAM if found, or else it calls an error."
  (let ((attrib (gl:get-attrib-location (id program) name)))
    (if (eql attrib -1)
        (error "Program attribute not found: ~a~%" name)
        attrib)))

(defun get-uniform (program name)
  "Return uniform-location NAME from PROGRAM if found, else call error."
  (declare (program program) (string name))
  (let ((uniform (gl:get-uniform-location (id program) name)))
    (if (eql uniform -1)
        (error "Program uniform not found: ~a~%" name)
        uniform)))

(defun make-program (vert-path frag-path &optional (geo-path nil))
  "Returns a new program instance."
  (let ((program (make-instance 'program)))
    (program-compile program vert-path frag-path geo-path)
    program))

;;; texture2d

(defclass texture2d ()
  ((id
    :accessor id
    :type unsigned-byte
    :initarg :id)
   (width
    :type unsigned-byte
    :accessor width
    :initarg :width)
   (height
    :type unsigned-byte
    :accessor height
    :initarg :height)
   (internal-format
    :accessor internal-format
    :initarg :internal-format)
   (image-format
    :accessor image-format
    :initarg :image-format)
   (wrap-s
    :accessor wrap-s
    :initarg :wrap-s)
   (wrap-t
    :accessor wrap-t
    :initarg :wrap-t)
   (filter-min
    :accessor filter-min
    :initarg :filter-min)
   (filter-max
    :accessor filter-max
    :initarg :filter-max))
  (:default-initargs
   :id (elt (gl:gen-textures 1) 0)
   :width 0
   :height 0
   :internal-format :rgb
   :image-format :rgb
   :wrap-s :repeat
   :wrap-t :repeat
   :filter-min :linear
   :filter-max :linear))

(defmethod initialize-instance :after ((tex texture2d) &key)
  t)

(defmethod texture2d-generate ((tex texture2d) tex-width
                               tex-height image)
  (with-slots (width height id
               wrap-s wrap-t
               filter-min filter-max
               internal-format image-format) tex
    (setf width tex-width
          height tex-height)
    (gl:bind-texture :texture-2d id)
    (gl:tex-image-2d :texture-2d 0 internal-format
                     width height 0 image-format
                     :unsigned-byte image)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-min-filter filter-min)
    (gl:tex-parameter :texture-2d :texture-mag-filter filter-max)
    (gl:bind-texture :texture-2d 0)))

(defmethod bind ((tex texture2d))
  (gl:bind-texture :texture-2d (id tex)))

(defun make-texture2d (filepath &optional (alpha t))
  "Returns texture2d instance."
  (declare (boolean alpha))
  (let ((texture2d (make-instance 'texture2d)))
    (with-accessors ((internal-format internal-format)
                     (image-format image-format)) texture2d
      (when alpha
        (setf internal-format :rgba
              image-format :rgba))
      (destructuring-bind (image width height channels)
          (cl-soil:load-image filepath image-format)
        (declare (ignore channels))
        (texture2d-generate texture2d width height image)
        (cl-soil:free-image-data image)))
    texture2d))

;;; resource manager

(defclass resource-manager ()
  ((resources
    :type list
    :initarg :resources
    :accessor resources))
  (:default-initargs
   :resources nil))

(defclass program-manager (resource-manager)
  ())

(defclass texture-manager (resource-manager)
  ())

(defgeneric load-resource (name resource manager )
  (:documentation "Loads RESOURCE into MANAGER. Can be retrieved with NAME."))
(defgeneric get-resource (name manager )
  (:documentation "Returns RESOURCE with key NAME, if it can be found, otherwise nil."))
(defgeneric clear-resources (manager)
  (:documentation "Cleans up all resources and empties RESOURCES."))

(defmethod initialize-instance :after ((manager resource-manager) &key)
  t)

(defmethod load-resource (name resource (manager resource-manager))
  (push (cons name resource) (resources manager)))

(defun load-texture (name resource &optional (mananger *texture-manager*))
  (load-resource name resource mananger))
(defun load-program (name resource &optional (mananger *program-manager*))
  (load-resource name resource mananger))

(defmethod get-resource (name (manager resource-manager))
  (let ((resource (cdr (assoc name (resources manager)
                              :test (if (stringp name) #'string= #'eql)))))
    (if resource
        resource
        (warn "\"~a\" not found in ~a.~%" name manager))))

(defun get-texture (name &optional (manager *texture-manager*))
  (get-resource name manager))
(defun get-program (name &optional (manager *program-manager*))
  (get-resource name manager))

(defmethod clear-resources :after ((manager resource-manager))
  (setf (resources manager) '()))

(defmethod clear-resources ((manager program-manager))
  ;; (mapcar (lambda (resource) (gl:delete-program (id (cdr resource))) (resources manager)))
  (iter (for (name . resource) in (resources manager))
    (gl:delete-program (id resource))))

(defmethod clear-resources ((manager texture-manager))
  (gl:delete-textures
   (mapcar (lambda (resource) (id (cdr resource))) (resources manager))
   ;; (iter (for (name . resource) in (resources manager))
   ;;   (collect (id resource)))
   ))

;;; renderers

(defclass renderer ()
  ((program
    :type program
    :accessor program
    :initarg :program)
   (vao
    :type unsigned-byte
    :accessor vao
    :initarg :vao))
  (:default-initargs
   :program nil
   :vao (gl:gen-vertex-array)))

(defclass sprite-renderer (renderer)
  ())

(defmethod initialize-instance :after ((renderer sprite-renderer) &key)
  (with-accessors ((vao vao)) renderer
    (let ((vbo (car (gl:gen-buffers 1))))
      ;; use vao from renderer
      (gl:bind-vertex-array vao)

      ;; upload buffer data from vertex buffer object
      (gl:bind-buffer :array-buffer vbo)
      (with-sequence-to-gl-array (verts
                                  ;;      Pos     Tex
                                  (vector 0.0 0.0 0.0 0.0
                                          1.0 0.0 1.0 0.0
                                          0.0 1.0 0.0 1.0
                                          1.0 1.0 1.0 1.0)
                                  :float)
        (gl:buffer-data :array-buffer :static-draw verts))

      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 4 :float nil (sizeof* :float 4) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)

      ;; clean up
      (gl:delete-buffers (list vbo)))))

(defmethod sprite-render ((texture2d texture2d)
                          (position vector)
                          &optional
                            (size (kit.glm:vec2 10.0 10.0))
                            (color (kit.glm:vec4 1.0 1.0 1.0 1.0))
                            (rotate 0.0)
                            (renderer *sprite-renderer*))
  (with-accessors ((program program) (vao vao)) renderer
    (use program)

    (gl:uniformfv (get-uniform program "spriteColor") color)
    (let ((model (kit.glm:matrix*
                  ;;finally move to POSITION
                  (kit.glm:translate (kit.glm:vec3 (aref position 0) (aref position 1) 0.0))
                  ;; move top left to 0.0, 0.0
                  (kit.glm:translate* (cfloat (* 0.5 (aref size 0)))
                                      (cfloat (* 0.5 (aref size 1)))
                                      0.0)
                  ;; rotate around the z-axis
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  ;; move sprite so that its center is at 0.0, 0.0, 0.0
                  (kit.glm:translate* (cfloat (* -0.5 (aref size 0)))
                                      (cfloat (* -0.5 (aref size 1)))
                                      0.0)
                  ;; scale first, z axis remain constant since 2d
                  (kit.glm:scale (kit.glm:vec3 (aref size 0) (aref size 1) 0.0)))))

      ;; set model uniform
      (gl:uniform-matrix-4fv (get-uniform program "model") (vector model) nil))

    ;; bind TEXTURE2D
    (gl:active-texture :texture0)
    (bind texture2d)

    ;; draw
    (gl:bind-vertex-array vao)
    (gl:draw-arrays :triangle-strip 0 4)
    (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)))

(defclass rect-renderer (renderer)
  ())

(defmethod initialize-instance :after ((renderer rect-renderer) &key)
  (with-slots (vao) renderer
    (let ((vbo (car (gl:gen-buffers 1))))
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      (with-sequence-to-gl-array (verts
                                  (vector 0.0 0.0
                                          1.0 0.0
                                          0.0 1.0
                                          1.0 1.0)
                                  :float)
        (gl:buffer-data :array-buffer :static-draw verts))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 2 :float nil (sizeof* :float 2) 0)

      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)

      (gl:delete-buffers (list vbo)))))

(defmethod rect-render ((position vector)
                        &optional
                          (size (kit.glm:vec2 10.0 10.0))
                          (color (kit.glm:vec4 1.0 1.0 1.0 1.0))
                          (rotate 0.0)
                          (renderer *rect-renderer*))
  (with-accessors ((program program) (vao vao)) renderer
    (use program)

    (gl:uniformfv (get-uniform program "rectColor") color)
    (let ((model (kit.glm:matrix*
                  ;;finally move to POS
                  (kit.glm:translate (kit.glm:vec3 (aref position 0) (aref position 1) 0.0))
                  ;; move top left to 0.0, 0.0
                  (kit.glm:translate* (cfloat (* 0.5 (aref size 0)))
                                      (cfloat (* 0.5 (aref size 1)))
                                      0.0)
                  ;; rotate around the z-axis
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  ;; move center to 0.0, 0.0, 0.0
                  (kit.glm:translate* (cfloat (* -0.5 (aref size 0)))
                                      (cfloat (* -0.5 (aref size 1)))
                                      0.0)
                  ;; scale first, z axis remain constant since 2d
                  (kit.glm:scale (kit.glm:vec3 (x-val size) (y-val size) 0.0)))))

      ;; set model uniform
      (gl:uniform-matrix-4fv (get-uniform program "model") (vector model) nil))

    ;; draw
    (gl:bind-vertex-array vao)
    (gl:draw-arrays :triangle-strip 0 4)
    (gl:bind-vertex-array 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; entities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((id 0))
  (defun make-entity (components &optional (entities *entities*))
    (incf id)
    (values (with entities id components) id)))

(defun add-entity (components &optional (entities *entities*))
  (setf *entities* (make-entity components entities)))

;; (defun add-entities (entity-list &optional (entities *entities*))
;;   (append entities entity-list))

(defun remove-entity (id &optional (entities *entities*))
  (less entities id))
(defmacro remove-entities (ids &optional (entities *entities*))
  `(-> ,entities ,@(mapcar (lambda (id) `(less ,id)) ids)))

(defun get-component (component id &optional (entities *entities*))
  (@ (@ entities id) component))
(defun set-component  (component id value &optional (entities *entities*))
  (with entities id (with (@ entities id) component value)))

;; (defmacro set-component (component entity-id value)
;;   `(setf ,components (plist-set ,components ,component ,value)))
;; (defun (setf get-component) (value component entity-id &optional (entities *entities*))
;;   (setf (cdr (nth entity-id entities))))
;; ;; (defun (setf get-component) (value component components)
;; ;;   (setf components (plist-set components component value)))

(defun get-entity (id &optional (entities *entities*))
  (@ entities id))
(defun set-entity (id value  &optional (entities *entities*))
  (with entities id value))

(defun get-entity-component (component entity)
  (@ entity component))
(defun set-entity-component (component entity value)
  (with entity component value))

(defun find-entities (predicate &optional (entities *entities*))
  (get-map-keys 'list (filter predicate entities)))

(defun find-entity-by-component (components &optional (entities *entities*))
  (let ((found ()))
    (do-map (x y entities)
      ;; go through COMPONENTS to see if they are in the current entity
      ;; if one is not then return NIL, else return T
      (when (iter (for c in components)
              (when (null (nth-value 1 (@ y c))) (return nil))
              (finally (return t)))
        (push x found)))
    (reverse found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-event (func)
  (alexandria:appendf *destructive-changes* (list func)))

(defun update-events ()
  (mapcar #'funcall *destructive-changes*)
  (setf *destructive-changes* nil)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; levels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defenum:defenum *enum-level-state* ((+level-play+ 0)
                                     +level-completed+))

(defenum:defenum *enum-rotation-state* ((+rotation-up+ 0)
                                        +rotation-left+
                                        +rotation-down+
                                        +rotation-right+))

(defglobal *level-state* +level-play+)
(defglobal *grid* (empty-map))
(defglobal *final-grid* ())
(defglobal *selected-matrix* 0)
(defglobal *matrices* (empty-seq))
(defglobal *difficulty* 2)

(defun make-matrix (rows cols)
  (map (:rows rows)
       (:cols cols)
       (:data (with (with-default (empty-seq) 0) (1- (* rows cols)) 0))
       (:x 0)
       (:y 0)
       (:rotation-state +rotation-up+)
       (:highlight-color (vec4 0.0 0.0 0.0 0.0))))

(defun matrix-rows (m)
  (@ m :rows))
(defun matrix-cols (m)
  (@ m :cols))

(defun matrix-at (m row col)
  (@ (@ m :data) (+ (* row (matrix-cols m)) col)))

(defun with-matrix-at (m row col value)
  (with m :data (with (@ m :data) (+ (* row (matrix-cols m)) col) value)))

(defun matrix-size (m)
  (size (@ m :data)))

(defun matrix-rotate-ccw (m &optional (rotations 1))
  (iter (for r from 0 below rotations)
    (let* ((rows (matrix-rows m))
           (cols (matrix-cols m))
           (new-m (with m :rotation-state (mod (1+ (@ m :rotation-state)) 4))))
      (iter (for i from 0 below rows)
        (iter (for j from 0 below cols)
          (setf new-m (with-matrix-at new-m (- cols 1 j) i (matrix-at m i j)))))
      (setf m new-m)))
  m)

(defun matrix-rotate-cw (m &optional (rotations 1))
  (iter (for r from 0 below rotations)
    (let* ((rows (matrix-rows m))
           (cols (matrix-cols m))
           (new-m (with m :rotation-state (mod (1- (@ m :rotation-state)) 4))))
      (iter (for i from 0 below rows)
        (iter (for j from 0 below cols)
          (setf new-m (with-matrix-at new-m j (- rows 1 i) (matrix-at m i j)))))
      (setf m new-m)))
  m)

(defun matrix-overlap (bottom top)
  (iter (for i from 0 below (matrix-rows top))
    (iter (for j from 0 below (matrix-cols top))
      (let ((row-pos (+ i (@ top :y)))
            (col-pos (+ j (@ top :x))))
        (setf bottom (with-matrix-at bottom row-pos col-pos
                       (mod (+ (matrix-at top i j)
                               (matrix-at bottom row-pos col-pos)) 2))))))
  bottom)

(defun matrix-data-equal? (a b)
  (let ((ad (@ a :data))
        (bd (@ b :data)))
    (when (= (size ad) (size bd))
      (let ((result t))
        (iter (for i from 0 below (size ad))
          (when (not (= (@ ad i) (@ bd i)))
            (setf result nil)
            (leave)))
        result))))

(defun update-grid ()
  (let ((grid (make-matrix (matrix-rows *grid*) (matrix-cols *grid*))))
    (do-seq (m *matrices*)
      (setf grid (matrix-overlap grid m)))
    (setf *grid* grid)))

(defun make-level (&optional (difficulty 3))
  (let ((dim (random-in-range difficulty (round (* 1.7 difficulty)))))
    (setf *grid* (make-matrix dim dim)
          *final-grid* *grid*
          *matrices* (empty-seq)
          *selected-matrix* 0)

    ;; matrices
    (iter (for i from 0 below (round (* difficulty 1.3)))
      (let* ((m-dim (random-in-range 2 dim)) ;; matrix dimension
             ;; between 1 and half of the matrix set
             (el-set (random-in-range 1 (round (/ (square m-dim) 2.0)))) 
             (m (make-matrix m-dim m-dim))
             (data (@ m :data)))

        ;; generate random matrix data
        (iter (while (> el-set 0))
          (iter (for i from 0 below m-dim) (while (> el-set 0))
            (iter (for j from 0 below m-dim) (while (> el-set 0))
              (let* ((set? (= 1 (random-in-range 1 (square m-dim))))
                     (value (cond (set?
                                   (decf el-set)
                                   1)
                                  (t
                                   0))))
                (setf data (with data (+ (* i m-dim) j) value))))))
        ;; give matrix random position
        (setf m (-> m
                    (with :data data)
                    (with :x (random-in-range 0 (- (matrix-cols *grid*) m-dim)))
                    (with :y (random-in-range 0 (- (matrix-rows *grid*) m-dim)))
                    (with :highlight-color (vec4 (random-in-range 0.0 0.6)
                                                 (random-in-range 0.0 0.6)
                                                 (random-in-range 0.0 0.6)
                                                 0.0))))

        ;; add to the final grid configuration
        (let* ((final-x (random-in-range 0 (- (matrix-cols *grid*) m-dim)))
               (final-y (random-in-range 0 (- (matrix-rows *grid*) m-dim)))
               (n-rotations (random-in-range 0 3))
               (final-m (-> m
                            (with :x final-x)
                            (with :y final-y)
                            (matrix-rotate-ccw n-rotations))))
          (setf *final-grid* (matrix-overlap *final-grid* final-m)))

        ;; add new matrix
        (setf *matrices* (with-last *matrices* m)))))
  (update-grid))

;;; game
(defenum:defenum *enum-game-state* ((+game-menu+ 0)
                                    +game-play+
                                    +game-help+))

(defglobal *game-state* +game-menu+)

(defun init ()
  (setf *random-state* (make-random-state t))
  (let ((sprite-program (make-program #p"./data/shaders/sprite.v.glsl"
                                      #p"./data/shaders/sprite.f.glsl"))
        (rect-program (make-program #p"./data/shaders/rect.v.glsl"
                                    #p"./data/shaders/rect.f.glsl")))
    (setf *program-manager* (make-instance 'program-manager)
          *texture-manager* (make-instance 'texture-manager)
          *sprite-renderer* (make-instance 'sprite-renderer :program sprite-program)
          *rect-renderer* (make-instance 'rect-renderer :program rect-program))
    (load-program "sprite" sprite-program)
    (load-program "rect" rect-program)

    ;; textures
    (load-texture "menu"
                  (make-texture2d "./data/images/menu.png" t))
    (load-texture "help"
                  (make-texture2d "./data/images/help.png" t))
    (load-texture "complete"
                  (make-texture2d "./data/images/complete.png" t))

    ;; use current program
    (use sprite-program)

    ;;set image uniform to texture0
    (gl:uniformi (get-uniform sprite-program "image") 0)

    ;; set projection matrix
    (gl:uniform-matrix-4fv
     (get-uniform sprite-program "projection")
     (vector
      ;; left, right, bottom, top, near, far
      (kit.glm:ortho-matrix 0.0
                            (cfloat *width*)
                            (cfloat *height*)
                            0.0
                            -1.0 1.0))
     ;; (vector (kit.math:perspective-matrix (kit.glm:deg-to-rad 45.0)
     ;;                               (cfloat (/ *width* *height*))
     ;;                               -2.1
     ;;                               100.0))
     nil)

    (use rect-program)
    (gl:uniform-matrix-4fv (get-uniform rect-program "projection")
                           (vector (kit.glm:ortho-matrix
                                    0.0
                                    (cfloat *width*)
                                    (cfloat *height*)
                                    0.0
                                    -1.0 1.0))
                           nil))
  (track-vars *game-state*
              *level-state*
              *difficulty*
              *grid*
              *final-grid*
              *selected-matrix*
              *matrices*))

(defun exit-to-menu ()
  (setf *game-state* +game-menu+)
  (setf *difficulty* 2))

(defun handle-input ()
  ;;debugging
  (when (or *key-actions* *mouse-button-actions*)
    (format t "keys : ~a | mouse : ~a~%" *key-actions* *mouse-button-actions*))

  ;; keys
  (when (key-action-p :escape :press)
    (cond ((eql *game-state* +game-menu+)
           (glfw:set-window-should-close))
          ((eql *game-state* +game-help+)
           (exit-to-menu))
          ((eql *game-state* +game-play+)
           (exit-to-menu))))

  (when (key-action-p :f1 :press)
    (setf *debug* (not *debug*)))

  (when *debug*
    (when (key-action-p :q :press)
      (rewind-pressed)) 
    (when (key-action-p :w :press)
      (forward-pressed))
    (when (key-action-p :e :press)
      (pause-pressed))
    (when (key-action-p :r :press)
      (play-pressed)))

  (when (eql *time-travel-state* +time-play+)
    ;; (when (key-action-p :c :press)
    ;;   (setf *entities* nil))

    ;; ;; mouse buttons
    ;; (when (mouse-button-action-p :left :press)
    ;;   (let* ((rect-width (/ *width* (matrix-cols *grid*)))
    ;;          (rect-height (/ *height* (matrix-rows *grid*)))
    ;;          (col-pos (floor (/ *cursor-x* rect-width)))
    ;;          (row-pos (floor (/ *cursor-y* rect-height))))
    ;;     (format t "~d ~d ~%" col-pos row-pos)
    ;;     (add-event (lambda ()
    ;;                  (setf *grid* (with-matrix-at *grid* row-pos col-pos
    ;;                                 (mod (1+ (matrix-at *grid* row-pos col-pos)) 2)))))))

    ;; when key press on menu screen create new level
    (when (eql *game-state* +game-menu+)
      (when (key-action-p :space :press)
        (setf *game-state* +game-play+)
        (make-level *difficulty*))
      (when (or (key-action-p :left-shift :press)
                (key-action-p :right-shift :press))
        (setf *game-state* +game-help+)
        (make-level *difficulty*)))

    ;; go back to old game board
    (when (eql *game-state* +game-help+)
      (when (key-action-p :space :press)
        (setf *game-state* +game-play+)))

    (when (eql *game-state* +game-play+)
      (when (eql *level-state* +level-play+)

        ;;; rotate
        (when (key-action-p :f :press)
          (add-event (lambda ()
                       (setf *matrices*
                             (with *matrices*
                                   *selected-matrix*
                                   (matrix-rotate-ccw
                                    (@ *matrices* *selected-matrix*)))))))

        (when (key-action-p :v :press)
          (add-event (lambda ()
                       (setf *matrices*
                             (with *matrices*
                                   *selected-matrix*
                                   (matrix-rotate-cw
                                    (@ *matrices* *selected-matrix*)))))))

        ;;; switch
        (when (key-action-p :z :press)
          (add-event (lambda ()
                       (setf *selected-matrix* (mod (1- *selected-matrix*)
                                                    (size *matrices*))))))

        (when (key-action-p :x :press)
          (add-event (lambda ()
                       (setf *selected-matrix* (mod (1+ *selected-matrix*)
                                                    (size *matrices*))))))

        ;;; move
        (when (key-action-p :up :press)
          (let ((selected (@ *matrices* *selected-matrix*)))
            (when (>= (1- (@ selected :y)) 0)
              (add-event (lambda ()
                           (setf *matrices*
                                 (with *matrices* *selected-matrix*
                                       (with selected :y (1- (@ selected :y))))))))))

        (when (key-action-p :down :press)
          (let ((selected (@ *matrices* *selected-matrix*)))
            (when (<= (+ (matrix-rows selected) (@ selected :y) 1) (matrix-rows *grid*))
              (add-event (lambda ()
                           (setf *matrices*
                                 (with *matrices* *selected-matrix*
                                       (with selected :y (1+ (@ selected :y))))))))))

        (when (key-action-p :left :press)
          (let ((selected (@ *matrices* *selected-matrix*)))
            (when (>= (1- (@ selected :x)) 0)
              (add-event (lambda ()
                           (setf *matrices*
                                 (with *matrices* *selected-matrix*
                                       (with selected :x (1- (@ selected :x))))))))))

        (when (key-action-p :right :press)
          (let ((selected (@ *matrices* *selected-matrix*)))
            (when (<= (+ (matrix-cols selected) (@ selected :x) 1) (matrix-cols *grid*))
              (add-event (lambda ()
                           (setf *matrices*
                                 (with *matrices* *selected-matrix*
                                       (with selected :x (1+ (@ selected :x))))))))))

        ;;; next level
        (when (key-action-p :n :press)
          (setf *level-state* +level-completed+)))

      ;;; help menu
      (when (or (key-action-p :left-shift :press)
                (key-action-p :right-shift :press))
        (setf *game-state* +game-help+)))))

(defun level-completed? ()
  (matrix-data-equal? *grid* *final-grid*))

(let ((time 1.0)
      (accum 0.0))
  (defun level-complete-pause-over? (dt)
    (incf accum dt)
    (let ((result (>= accum time)))
      (when result
        (setf accum 0.0))
      result)))

(defun update ()
  (cond ((eql *time-travel-state* +time-play+)
         (update-timeline)
         (when (eql *game-state* +game-play+)
           (when (eql *level-state* +level-play+)
             (update-events)
             (update-grid)
             (when (level-completed?)
               (setf *level-state* +level-completed+)))
           (when (eql *level-state* +level-completed+)
             (when (level-complete-pause-over? *dt*)
               (setf *level-state* +level-play+)
               (make-level (incf *difficulty*))))))
        ((eql *time-travel-state* +time-rewind+)
         (rewind-time))
        ((eql *time-travel-state* +time-forward+)
         (forward-time))))

(defun render-grid ()
  (let* ((line-thickness 2.0)
         (line-thickness/2 (/ line-thickness 2.0))
         (rect-width (cfloat (/ *width* (matrix-cols *grid*))))
         (rect-height (cfloat (/ *height* (matrix-rows *grid*)))))
    ;; (iter (for j from 0 to (matrix-cols *grid*))
    ;;   ;; vertical grid lines
    ;;   (rect-render (vec2 (- (* j rect-width) line-thickness/2) 0.0)
    ;;                (vec2 line-thickness (cfloat *height*))
    ;;                (vec4 1.0 1.0 1.0 0.5)
    ;;                0.0))

    ;; (iter (for i from 0 to (matrix-rows *grid*))
    ;;   ;;horizontal
    ;;   (rect-render (vec2 0.0 (- (* i rect-height) line-thickness/2))
    ;;                (vec2 (cfloat *width*) line-thickness)
    ;;                (vec4 1.0 1.0 1.0 0.5)
    ;;                0.0))

    (iter (for i from 0 below (matrix-rows *grid*))
      (iter (for j from 0 below (matrix-cols *grid*))
        (rect-render (vec2 (* j rect-width) (* i rect-height))
                     (vec2 rect-width rect-height)
                     (cond ((= (matrix-at *final-grid* i j) 1)
                            (if (= (matrix-at *grid* i j)
                                   (matrix-at *final-grid* i j))
                                (vec4 0.0 1.0 0.0 0.6)
                                (vec4 1.0 0.0 0.0 0.6)))
                           (t
                            (case (matrix-at *grid* i j)
                              (0
                               (vec4 0.0 0.0 0.0 0.0))
                              (1
                               (vec4 1.0 1.0 1.0 0.9)))))
                     0)))))

(defun render-matrix (matrix line-thickness color size-div)
  (let* ((line-thickness/2 (/ line-thickness 2.0))
         (rect-width (cfloat (/ *width* (matrix-cols *grid*))))
         (rect-height (cfloat (/ *height* (matrix-rows *grid*))))
         (swidth (* rect-width size-div))
         (sheight (* rect-height size-div))
         (x (@ matrix :x))
         (y (@ matrix :y))
         (cols (matrix-cols matrix))
         (rows (matrix-rows matrix))
         (rotation-state (@ matrix :rotation-state))
         (angle (cond ((eql rotation-state +rotation-up+)
                       0.0)
                      ((eql rotation-state +rotation-left+)
                       (/ pi 2))
                      ((eql rotation-state +rotation-down+)
                       (/ pi 3))
                      ((eql rotation-state +rotation-right+)
                       (/ pi -5)))))
    ;;vertical
    (iter (for j from x to (+ x cols))
      (rect-render (vec2 (- (* j rect-width) line-thickness/2) (* y rect-height))
                   (vec2 line-thickness (* rows rect-height))
                   color))

    ;;horizontal lines
    (iter (for i from y to (+ y rows))
      (rect-render (vec2 (* x rect-width) (- (* i rect-height) line-thickness/2))
                   (vec2 (* cols rect-width) line-thickness)
                   color))

    (iter (for i from 0 below rows)
      (iter (for j from 0 below cols)
        (when (= (matrix-at matrix i j) 1)
          ;; render an inner triangle
          (rect-render
           (vec2 (+ (* x rect-width) ;; x position of grid
                    (* j rect-width) ;; x position of square
                    (/ (- rect-width swidth) 2.0) ;; x of inner object
                    (* swidth 0.32))
                 (+ (* y rect-height) ;; y of grid
                    (* i rect-height) ;; y of square
                    (/ (- rect-height sheight) 2.0) ;; y of inner object
                    ))
           (vec2 line-thickness sheight)
           color
           (+ angle (/ pi 7.0)))
          (rect-render
           (vec2 (+ (* x rect-width) ;; x position of grid
                    (* j rect-width) ;; x position of square
                    (/ (- rect-width swidth) 2.0) ;; x of inner object
                    (* swidth 0.7))
                 (+ (* y rect-height) ;; y of grid
                    (* i rect-height) ;; y of square
                    (/ (- rect-height sheight) 2.0) ;; y of inner object
                    ))
           (vec2 line-thickness sheight)
           color
           (+ angle (/ pi -7.0)))
          (rect-render
           (vec2 (+ (* x rect-width) ;; x position of grid
                    (* j rect-width) ;; x position of square
                    (/ (- rect-width swidth) 2.0) ;; x of inner object
                    (/ swidth 2))
                 (+ (* y rect-height) ;; y of grid
                    (* i rect-height) ;; y of square
                    (/ (- rect-height sheight) 2.0) ;; y of inner object
                    (/ sheight 2)
                    ))
           (vec2 line-thickness sheight)
           color
           (+ angle (/ pi 2))))))))

(defun render-selected-grid ()
  (render-matrix (@ *matrices* *selected-matrix*)
                 3.0
                 (vec4 0.0 0.0 1.0 0.9)
                 0.4))

(defun render-proximity-highlight ()
  (let* ((prev (@ *matrices* (mod (1- *selected-matrix*) (size *matrices*))))
         (prev-color (vec4 0.8 0.2 0.0 0.7))
         (next (@ *matrices* (mod (1+ *selected-matrix*) (size *matrices*))))
         (next-color (vec4 1.0 0.0 1.0 0.7)))
    (render-matrix prev 4.0 prev-color 0.2)
    (render-matrix next 5.0 next-color 0.3)))

(defun render-grid-highlight ()
  (iter (for m from 0 below (size *matrices*))
    (when (= m *selected-matrix*) (next-iteration))
    (let* ((current (@ *matrices* m))
           (current-dist (dist-mod m *selected-matrix* (size *matrices*)))
           (highlight-color (@ current :highlight-color))
           ;; (highlight-alpha 0.8)
           (current-color (vec4 (x-val highlight-color)
                                (y-val highlight-color)
                                (z-val highlight-color)
                                ;; (/ highlight-alpha current-dist)
                                0.8)))
      (render-matrix current
                     (/ 8.0 current-dist)
                     current-color
                     (/ 0.5 current-dist)))))

;; (defun render-entities (&optional (entities *entities*))
;;   t)

(defun render-menu ()
  (let* ((dim 10)
         (line-thickness 2.0)
         (line-thickness/2 (/ line-thickness 2.0))
         (line-color (vec4 1.0 1.0 1.0 0.3))
         (rect-width (cfloat (/ *width* dim)))
         (rect-height (cfloat (/ *height* dim))))

    (iter (for i from 0 to dim)
      ;; vertical grid lines
      (rect-render (vec2 (- (* i rect-width) line-thickness/2) 0.0)
                   (vec2 line-thickness (cfloat *height*))
                   line-color)

      ;;horizontal
      (rect-render (vec2 0.0 (- (* i rect-height) line-thickness/2))
                   (vec2 (cfloat *width*) line-thickness)
                   line-color)))

  (sprite-render (get-texture "menu")
                 (vec2 0.0 0.0)
                 (vec2 (cfloat *width*) (cfloat *height*))
                 (vec4 1.0 1.0 1.0 1.0)))

(defun render-help ()
  (sprite-render (get-texture "help")
                 (vec2 0.0 0.0)
                 (vec2 (cfloat *width*) (cfloat *height*))
                 (vec4 1.0 1.0 1.0 1.0)))

(defun render-complete-screen ()
  (sprite-render (get-texture "complete")
                 (vec2 0.0 0.0)
                 (vec2 (cfloat *width*) (cfloat *height*))
                 (vec4 1.0 1.0 1.0 0.8)))

(defun render ()
  (when (not (eql *time-travel-state* +time-paused+))
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer-bit)
    (when (eql *game-state* +game-play+)
      ;; (when (eql *level-state* +level-play+))
      (render-grid)
      ;; highlight grids
      (if (key-pressed-p :d)
          (render-grid-highlight)
          (render-proximity-highlight))
      (render-selected-grid)
      (when (eql *level-state* +level-completed+)
        (render-complete-screen)))
    (when (eql *game-state* +game-menu+)
      (render-menu))
    (when (eql *game-state* +game-help+)
      (render-help))))

(defun cleanup ()
  (clear-resources *program-manager*)
  (clear-resources *texture-manager*)
  t)

(defun game ()
  (glfw:with-init-window (:title "mt"
                          :width *width*
                          :height *height*
                          :opengl-forward-compat t
                          :opengl-profile :opengl-core-profile
                          :context-version-major 3
                          :context-version-minor 3
                          :decorated t
                          :resizable nil
                          ;;full screen mode
                          ;; :monitor (glfw:get-primary-monitor)
                          ;; :refresh-rate 60
                          )
    ;; (glfw:swap-interval 1)
    (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

    ;; initialize
    (unless (gl::features-present-p (>= :glsl-version 3.3))
      ;;destroys the window cuz of unwind-protect
      (return-from game nil))

    (initialize-globals)
    (init)

    (gl:enable :blend)
    (gl:disable :depth-test)
    (gl:blend-func :src-alpha :one-minus-src-alpha)

    (glfw:set-key-callback 'key-callback)
    (glfw:set-mouse-button-callback 'mouse-callback)
    (glfw:set-cursor-position-callback 'cursor-callback)
    (glfw:set-scroll-callback 'scroll-callback)
    ;; (glfw:set-input-mode :cursor :disabled)

    (iter (until (glfw:window-should-close-p))
      (update-swank)
      ;; give some fps data in title
      (update-window-title cl-glfw3:*window* "mt")

      (glfw:poll-events)

      (handle-input)
      (render)
      (update)

      (glfw:swap-buffers)
      (update-globals))

    (cleanup)))
