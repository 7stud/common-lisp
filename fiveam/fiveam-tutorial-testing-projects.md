3-21-2024  
I'm using emacs 29.1, sbcl 2.4.0, and slime 2.29.1.

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
(defun greet (greeting)
  greeting)
```
   Then export the function:
```
(defpackage proj2
  (:use :cl)
  (:export :greet))   ;; <==== CHANGE HERE
(in-package :proj2)

;; blah blah blah.

;; I ADDED THE FOLLOWING:
(defun greet (greeting)
  greeting)
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

(test whether-greet-returns-the-proper-greeting    ;; The name of the test cannot be just
  (is (equal "goodbye" (greet "hello")))           ;; 'greet'.
  (is (equal "hello" (greet "hello")))
  (is (equal "mars" (greet "goodbye"))))
```

8. Create a function to run your tests:
```
(defun test-proj2 ()
  (run! 'master-suite))
```
`run!` is a function in the `fiveam` package. Put your function above your test suite:
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

(test whether-greet-returns-the-proper-greeting
  (is (equal "goodbye" (greet "hello")))
  (is (equal "hello" (greet "hello")))
  (is (equal "mars" (greet "goodbye"))))
```
9. Go to the file  `...proj2/proj2.asd` and change some of the configuration so that it looks like the
   the following (changes are marked with comments):

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
  :in-order-to ((test-op (test-op "proj2/tests"))))   ;; <=== Make sure this is correct

(defsystem "proj2/tests"
  :author ""
  :license ""
  :depends-on ("proj2"
               "fiveam")    ;; <====== CHANGE HERE
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for proj2"
  :perform (test-op (op c)
              (symbol-call :proj2/tests/main :test-proj2)))   ;; <===== CHANGE HERE
```

10.  Run the tests:

```
CL-USER> (asdf:test-system 'proj2)
; compiling file "/Users/7stud/quicklisp/local-projects/my-projects/proj2/tests/main.lisp" (written 21 MAR 2024 09:01:58 PM):

; wrote /Users/7stud/.cache/common-lisp/sbcl-2.4.0-macosx-arm64/Users/7stud/quicklisp/local-projects/my-projects/proj2/tests/main-tmpQ371UGST.fasl
; compilation finished in 0:00:00.006

Running test suite MASTER-SUITE
 Running test WHETHER-GREET-RETURNS-THE-PROPER-GREETING f.f   ;;  An f reprsents a failing test.
 Did 3 checks.                                                ;;  A dot represents a passing test.
    Pass: 1 (33%)
    Skip: 0 ( 0%)
    Fail: 2 (66%)

 Failure Details:
 --------------------------------
 WHETHER-GREET-RETURNS-THE-PROPER-GREETING in MASTER-SUITE []: 
      
(GREET "hello")

 evaluated to 

"hello"

 which is not 

EQUAL

 to 

"goodbye"


 --------------------------------
 --------------------------------
 WHETHER-GREET-RETURNS-THE-PROPER-GREETING in MASTER-SUITE []: 
      
(GREET "goodbye")

 evaluated to 

"goodbye"

 which is not 

EQUAL

 to 

"mars"


 --------------------------------

T
CL-USER> 
```
I think `asdf` is supposed to load your project into the repl, but I find that sometimes I get errors when
trying to run the tests after I've modified my project's files--even though I know I've corrected the
errors, and I've saved the files.  If you get persistent errors that you can't solve or that you don't 
understand after you've been editing any of your project's files, try reloading both of your 
project's "systems" in the repl:
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
And, if for some reason you suspect that `asdf` or `(ql:quickload ...)` can't find your "systems",
then try:
```
CL-USER> (ql:register-local-projects)
```
That will cause `quicklisp` to search through all the directories in `local-projects/` for `.asd` files,
and the paths to the `.asd `files it finds will be written into `local-projects/system-index.txt`.  If the
path to your "system" is not in that file, then `ql:quickload` will not be able to load your "system"
into the repl.  However, don't type any paths in that file yourself.

Finally, the following line in `proj2.asd` can cause a lot of headaches:

```
:perform (test-op (op c) (symbol-call :proj2/tests/main :test-proj2)))
```

If you don't define your own no-arg function, e.g. `test-proj2`, to run your tests, then the `:perform`
line needs to look like this:
```
  :perform (test-op (o s)
        (symbol-call :fiveam :run!
           (find-symbol* :master-suite :proj2/tests/main))))
```
The `:perform` line is read before any of the packages in your project are created.  The function 
`symbol-call` allows you to specify a package, `fiveam`, and a function in that package, `run!`, and 
the args for the function--when the package hasn't been created yet.  The args for the `run!` function are 
whatever the function `find-symbol*` returns.  `find-symbol*` lets you specify a symbol, `master-suite`, 
in a package, `proj1/tests/main` that hasn't been read yet.  None of the `fiveam` tutorials that I read 
has a `:perform` line that works.  I got the `:perform` line above from the `asdf` manual in the section 
describing the [test-op](https://asdf.common-lisp.dev/asdf/Predefined-operations-of-ASDF.html#index-test_002dop) function.  

For instance, the turtleware fiveam tutorial may work once (it didn't for me), but you won't 
be able to run the tetsts again if you quit the repl, then restart it.  Why?  In the tutorial, 
the author loads both "sytems" with `ql:quickload`, and thereafter changes the .asd file to add 
the `:perform` line.  That means all the packages were loaded into the repl *before* the :perform
line was read, therefore the `:perform` line won't have any issues with packages that don't exist.  But, 
if you restart the repl, then try to load the "systems" into the repl, the `:perform` line *will be* 
read before the packages exist, and then you will get "package XYZ doesn't exist" errors because the
:perform line wasn't written properly.  In essence, the tutorial mistakenly creates a test methodology 
that requires that you delete the `:perform` line in the .asd file, then load the "systems' into the repl 
with `ql:quickload`, then add the ":perform line back into the .asd file, then run the tests.  






