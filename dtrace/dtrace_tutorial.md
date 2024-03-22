I installed [quicklisp](https://www.quicklisp.org/beta/).  Then I put the `dtrace.lisp` file in:

```
~/quicklisp/local-projects/dtrace/
```

I had to create the `dtrace` directory.  Then I tried:

```
CL-USER> (ql:quickload "dtrace")
; Evaluation aborted on #<QUICKLISP-CLIENT:SYSTEM-NOT-FOUND {7007233D63}>.
CL-USER> 
```
 
Next, I added a `dtrace.asd` file to the directory  `~/quicklisp/local-projects/dtrace/ `:

```
(asdf:defsystem #:dtrace
    :components ((:file "dtrace")))
```

See: https://stackoverflow.com/questions/19718002/how-do-i-manage-common-lisp-dependencies

Then, in the slime repl (and it worked for me in the the sbcl repl, as well) I did:

```
CL-USER> (ql:quickload "dtrace")
To load "dtrace":
  Load 1 ASDF system:
    dtrace
; Loading "dtrace"

("dtrace")
CL-USER> 
```

Then I tried to trace a function called `anyoddp`:

```
CL-USER> (dtrace anyoddp)
ANYODDP undefined function.
NIL
; processing (DEFUN ANYODDP ...)
CL-USER>
```
Then I compiled the file containing `anyoddp`, `C-c C-k`, and I tried again:

```
CL-USER> (dtrace anyoddp)
WARNING: Function is not TRACEd: ANYODDP
(ANYODDP)
CL-USER>
```
Note there are NO QUOTES around the function name `anyoddp` above:

```
(dtrace anyoddp)
```

I've spent an hour typing in different function names with quotes only to get the blasted error: `""my-func" is an invalid function name`, then deleting/reinstalling quicklisp, configuring `dtrace` again only to get the same blasted error.  Learn from my mistake!

Then:

```
CL-USER> (anyoddp (list 2 4 5 6))
----Enter ANYODDP
|     Arg-1 = (2 4 5 6)
|   ----Enter ANYODDP
|   |     Arg-1 = (4 5 6)
|   |   ----Enter ANYODDP
|   |   |     Arg-1 = (5 6)
|   |    \--ANYODDP returned T
|    \--ANYODDP returned T
 \--ANYODDP returned T
T
CL-USER> (dtrace)
(ANYODDP)
CL-USER> (duntrace anyoddp)
(ANYODDP)
CL-USER> (dtrace)
NIL
CL-USER> 
```

Anytime you restart emacs, in the repl you will have to do:

```
(ql:quickload "dtrace")
```

Don't forget the quotes. 

If the steps above don't work for you, keep reading.  At some point, `quicklisp` should automatically create a file named:

```
~/quicklisp/local-projects/system-index.txt
```

and that file should contain:

```
dtrace/dtrace.asd
```

If that path isn't in the file, then in the repl do:

```
CL-USER> (ql:register-local-projects)
Nil
CL-USER>
```

That will cause quicklisp to search all the directories in `~/quicklisp/local-projects` and write the paths to all the .asd files it finds into `system-index.txt`.  See: http://blog.quicklisp.org/2018/01/the-quicklisp-local-projects-mechanism.html

Thereafter, in the repl you should be able to do:

```
CL-USER> (ql:quickload "dtrace")
To load "dtrace":
  Load 1 ASDF system:
    dtrace
; Loading "dtrace"

("dtrace")
CL-USER>
```
