I'm using emacs 29.1 and slime.

1. Install [quicklisp](https://www.quicklisp.org/beta/).

2. Install `fiveam`:
   ```
   CL-USER> (ql:quickload "fiveam")
   To load "fiveam":
    Load 1 ASDF system:
       fiveam
    ; Loading "fiveam"

    ("fiveam")
    CL-USER>
    ```
3. Install `cl-project` to help create a skeleton project:

   ```
   CL-USER> (ql:quickload "cl-project")
   To load "cl-project":
     Load 1 ASDF system:
        cl-project
     ; Loading "cl-project"
   ("cl-project")
   CL-USER>
   ```

   `ql:quickload` not only downloads software and its dependencies, it loads the software into the repl.
    If the software has already been downloaded, then `ql:quickload` just loads the software into the repl.

4. Create a project inside the directory `~/quicklisp/local-projects/`:
   ```
   CL-USER> (cl-project:make-project #p "~/quicklisp/local-projects/my-projects/proj2")
   writing ~/quicklisp/local-projects/my-projects/proj2/proj2.asd
   writing ~/quicklisp/local-projects/my-projects/proj2/README.org
   writing ~/quicklisp/local-projects/my-projects/proj2/README.markdown
   writing ~/quicklisp/local-projects/my-projects/proj2/.gitignore
   writing ~/quicklisp/local-projects/my-projects/proj2/src/main.lisp
   writing ~/quicklisp/local-projects/my-projects/proj2/tests/main.lisp
   T
   CL-USER>
   ```
   In this case, I named my project `proj2`.

   
5. Go to the file `.../proj2/src/main.lisp` and add a function you want to test, for instance:

```
(defpackage proj2
  (:use :cl))
(in-package :proj2)

;; blah blah blah.

;; I ADDED THE FOLLOWING:
(defun do-stuff ()
  "hello")
```
   Then export the function:
```
(defpackage proj2
  (:use :cl)
  (:export :do-stuff))   ;; <==== CHANGE HERE
(in-package :proj2)

;; blah blah blah.

;; I ADDED THE FOLLOWING:
(defun do-stuff ()
  "hello")
```

   

6. Go to the file `...proj2/tests/main.lisp` and change the `:use` declarations to the following:
```
(defpackage proj2/tests/main
  (:use :cl
        :proj2
        :fiveam))    ;; <==== CHANGED THIS from :rove to :fiveam
(in-package :proj2/tests/main)
```

7.  Add a test suite and add some tests to `...tests/main.lisp`:
```
(defpackage proj2/tests/main
  (:use :cl
        :proj2
        :fiveam))   ;; <==== CHANGED THIS from :rove to :fiveam
(in-package :proj2/tests/main)

;; I ADDED THE FOLLOWING:
(def-suite master-suite
  :description "Test my system.")
(in-suite master-suite)

(test do-stuff
  (is (equal "goodbye" (do-stuff)))
  (is (equal "hello" (do-stuff))))
```

8. Create a function to run your tests:
```
(defun test-proj2 ()
  (run! 'master-suite))
```
   and put it above your test suite:
```
(defpackage proj2/tests/main
  (:use :cl
        :proj2
        :fiveam))
(in-package :proj2/tests/main)

(defun test-proj2 ()     ;; <==== HERE
  (run! 'master-suite))

(def-suite master-suite
  :description "Test my system.")
(in-suite master-suite)

(test do-stuff
  (is (equal "goodbye" (do-stuff)))
  (is (equal "hello" (do-stuff))))
```

9. Go to the file  `...proj2/proj2.asd` and change some of the configuration so that it looks like the
   the following:

```
(defsystem "proj2"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "proj2/tests"))))

(defsystem "proj2/tests"
  :author ""
  :license ""
  :depends-on ("proj2"
               "fiveam")    ;; <====== CHANGE HERE
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for proj2"
  :perform (test-op (op c) (symbol-call :proj2/tests/main :test-proj2)))   ;; <===== CHANGE HERE
```

10.  To run the tests:

```
CL-USER> (asdf:test-system 'proj2)

Running test suite MASTER-SUITE
 Running test DO-STUFF-RETURN-VAL f.
 Did 2 checks.
    Pass: 1 (50%)
    Skip: 0 ( 0%)
    Fail: 1 (50%)

 Failure Details:
 --------------------------------
 DO-STUFF-RETURN-VAL in MASTER-SUITE []: 
      
(DO-STUFF)

 evaluated to 

"hello"

 which is not 

EQUAL

 to 

"goodbye"


 --------------------------------

T
CL-USER>
```

If you get persistent errors, like:

```
; Evaluation aborted on #<NAME-CONFLICT {70083FF8B3}>
```

after you've been editing any of your project's files to correct those very errors, then reload both 
"systems" in the repl:
```
CL-USER> (ql:quickload "proj2")
To load "proj2":
  Load 1 ASDF system:
    proj2
; Loading "proj2"
[package proj2]
("proj2")
CL-USER> (ql:quickload "proj2/tests")
To load "proj2/tests":
  Load 1 ASDF system:
    proj2/tests
; Loading "proj2/tests"
[package proj2/tests/main]
("proj2/tests")
```







