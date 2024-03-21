I'm using emacs 29.1 and slime.

1. Install [quicklisp](https://www.quicklisp.org/beta/).

2. Use `quicklisp` to install `fiveam` (a testing framework):
   ```
   CL-USER> (ql:quickload "fiveam")
   To load "fiveam":
    Load 1 ASDF system:
       fiveam
    ; Loading "fiveam"

    ("fiveam")
    CL-USER>
    ```
3. Use `quicklisp` to install `cl-project` to help create a skeleton project:

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

4. Use `cl-project` to create a project inside the directory `~/quicklisp/local-projects/`:
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

   
5. Go to the file `.../proj2/src/main.lisp` and add a function that you want to test, for instance:

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
`run!` is a function in the `fiveam` package. Put the function above your test suite:
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

10.  Run the tests:

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
that you know you've corrected, then try reloading both of your project's
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
Then try the following again:
```
CL-USER> (asdf:test-system 'proj2)
```

The following line can cause a lot of headaches:

```
:perform (test-op (op c) (symbol-call :proj2/tests/main :test-proj2)))
```

If you don't define your own function, `test-proj2`, to run your tests, then `:perform` line 
needs to look like this:
```
 :perform (test-op (o s)
       (symbol-call :fiveam '#:run!
          (find-symbol* '#:master-suite
                        :proj1/tests/main))))
```
That tortured syntax is necessary because the `:perform` line is read before any of the packages or symbols have 
been created in your project.  The function `symbol-call` allows you to specify a package, `fiveam`, and a 
function in that package, `run!`, and the args for the function--when the package hasn't been created yet.
The args for the `run!` function are whatever the function `find-symbol*` returns.  `find-symbol*` lets you
specify a symbol, `master-suite`, in a package, `proj1/tests/main` that hasn't been read yet.




