3-22-2024   
I'm using emacs 29.1, sbcl 2.4.0, and slime 2.29.1.

1. Install [quicklisp](https://www.quicklisp.org/beta/).

2. Here is a file that defines a package containing one function followed by some test suites:
```
;; c.lisp

(defpackage my-list-functions
  (:use :cl
        :fiveam))
(in-package my-list-functions)

(defun anyoddp (alist)
  (cond ((null alist) nil)
        ((oddp (first alist)) t)
        (t (anyoddp (rest alist)))))

(def-suite master-suite
  :description "All tests")

(def-suite simple-tests
  :description "Some simple tests"
  :in master-suite)
(in-suite simple-tests)

(test test1
  (is (= 2 (+ 1 1))))

(test test2
      (is (= 3 (+ 4 3))))

(def-suite anyoddp-tests
  :description "Test anyoddp"
  :in master-suite)
(in-suite anyoddp-tests)

(test anyoddp-with-list-of-evens
  (is (equal (anyoddp '(2 4 6)) nil))
  (is (equal (anyoddp '(2)) nil))
  (is (equal (anyoddp '()) nil)))

(test anyoddp-with-list-containing-odds
  (is (equal (anyoddp '(2 3 4)) t))
  (is (equal (anyoddp '(2 3)) t))
  (is (equal (anyoddp '(1)) t)))
```

3. Here's what I tried:
```
C-x C-s   save c.lisp
C-c C-k   compile c.lisp

The name "FIVEAM" does not designate any package.
   [Condition of type PACKAGE-DOES-NOT-EXIST]
```
Then:
```
CL-USER> (ql:quickload "fiveam")
To load "fiveam":
  Load 1 ASDF system:
    fiveam
; Loading "fiveam"

("fiveam")
CL-USER>
```
Then C-c C-k again in the c.lisp buffer. slime said:
```
; compiling file "/Users/7stud/lisp_programs/c.lisp" (written 20 MAR 2024 03:08:06 PM):

; wrote /Users/7stud/lisp_programs/c.fasl
; compilation finished in 0:00:00.009
```
Then:
```
CL-USER> (run! 'master-suite)
; in: RUN! 'MASTER-SUITE
;     (RUN! 'MASTER-SUITE)
; 
; caught STYLE-WARNING:
;   undefined function: COMMON-LISP-USER::RUN!
; 
; compilation unit finished
;   Undefined function:
;     RUN!
;   caught 1 STYLE-WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION RUN! {7005A44223}>.
CL-USER>
```
Then:
```
CL-USER> (fiveam:run! 'master-suite)
 Didn't run anything...huh?
T
NIL
NIL
CL-USER>
```
Then:
```
CL-USER> (in-package my-list-functions)
#<PACKAGE "MY-LIST-FUNCTIONS">

MY-LIST-FUNCTIONS> (run! 'master-suite)

Running test suite MASTER-SUITE
 Running test suite SIMPLE-TESTS
  Running test TEST1 .         ; <===== A dot represents a test that passed.
  Running test TEST2 f         ; <===== An f represents a test that failed.
 Running test suite ANYODDP-TESTS
  Running test ANYODDP-WITH-LIST-OF-EVENS ...
  Running test ANYODDP-WITH-LIST-CONTAINING-ODDS ...
 Did 8 checks.
    Pass: 7 (87%)
    Skip: 0 ( 0%)
    Fail: 1 (12%)

 Failure Details:
 --------------------------------
 TEST2 in SIMPLE-TESTS []: 
      
(+ 4 3)

 evaluated to 

7

 which is not 

=

 to 

3


 --------------------------------

NIL
(#<IT.BESE.FIVEAM::TEST-FAILURE {70064690A3}>)
NIL
MY-LIST-FUNCTIONS> 
```

And:
```
MY-LIST-FUNCTIONS> (run! 'anyoddp-tests)

Running test suite ANYODDP-TESTS
 Running test ANYODDP-WITH-LIST-OF-EVENS ...
 Running test ANYODDP-WITH-LIST-CONTAINING-ODDS ...
 Did 6 checks.
    Pass: 6 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
NIL
NIL
MY-LIST-FUNCTIONS> 
```
