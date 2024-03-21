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
3. To help create a skeleton project, install `cl-project`:

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

5. Create a project inside the directory `~/quicklisp/local-projects/`:
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

   

   
