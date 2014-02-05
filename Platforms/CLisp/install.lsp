;;CLisp Installation Windows
;;install and wipe away the junk

(PROCLAIM '(OPTIMIZE (DEBUG 0) (SPEED 0) (SAFETY 3)))
(SETQ CUSTOM:*COMPILE-WARNINGS* NIL) 
(SETQ *COMPILE-VERBOSE* NIL)  

(IN-PACKAGE :CL-USER)
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(SETQ *language* "Common Lisp")
(SETQ *implementation* "CLisp")
(SETQ *release* "2.49")
(SETQ *port* 1.6)
(SETQ *porters* "Mark Tarver")
(SETQ *os* "Windows 7")

(DEFUN clisp-install (File)
  (LET* ((Read (read-in-kl File))
         (Intermediate (FORMAT NIL "~A.intermed" File))
         (Delete (DELETE-FILE Intermediate))
         (Write (write-out-kl Intermediate Read)))
        (boot Intermediate)
        (LET ((Lisp (FORMAT NIL "~A.lsp" Intermediate)))
             (COMPILE-FILE Lisp)
             (LOAD (FORMAT NIL "~A.fas" Intermediate))   
             (DELETE-FILE Intermediate)
             (move-file Lisp)
             (DELETE-FILE (FORMAT NIL "~A.fas" Intermediate))
             (DELETE-FILE (FORMAT NIL "~A.lib" Intermediate))
             (DELETE-FILE File)))) 

(DEFUN move-file (Lisp) 
  (LET ((Rename (native-name Lisp)))
       (IF (PROBE-FILE Rename) (DELETE-FILE Rename))
       (RENAME-FILE Lisp Rename)))

(DEFUN native-name (Lisp) 
   (FORMAT NIL "Native/~{~C~}.native" 
          (nn-h (COERCE Lisp 'LIST)))) 

(DEFUN nn-h (Lisp)
  (IF (CHAR-EQUAL (CAR Lisp) #\.)
      NIL
      (CONS (CAR Lisp) (nn-h (CDR Lisp)))))

(DEFUN read-in-kl (File)
 (WITH-OPEN-FILE (In File :DIRECTION :INPUT)
   (kl-cycle (READ-CHAR In NIL NIL) In NIL 0)))
   
(DEFUN kl-cycle (Char In Chars State)
  (COND ((NULL Char) (REVERSE Chars))
        ((AND (MEMBER Char '(#\: #\; #\,) :TEST 'CHAR-EQUAL) (= State 0))
         (kl-cycle (READ-CHAR In NIL NIL) In (APPEND (LIST #\| Char #\|) Chars) State))
       ((CHAR-EQUAL Char #\") (kl-cycle (READ-CHAR In NIL NIL) In (CONS Char Chars) (flip State)))
        (T (kl-cycle (READ-CHAR In NIL NIL) In (CONS Char Chars) State))))

(DEFUN flip (State)
  (IF (ZEROP State)
      1
      0)) 

(COMPILE 'read-in-kl)
(COMPILE 'kl-cycle)   
(COMPILE 'flip)
   
(DEFUN write-out-kl (File Chars)
  (WITH-OPEN-FILE (Out File :DIRECTION :OUTPUT 
                            :IF-EXISTS :OVERWRITE 
                            :IF-DOES-NOT-EXIST :CREATE)
   (FORMAT Out "~{~C~}" Chars)))

(COMPILE 'write-out-kl)

(COMPILE-FILE "primitives.lsp")
(LOAD "primitives.fas")
(DELETE-FILE "primitives.fas")
(DELETE-FILE "primitives.lib")   

(COMPILE-FILE "backend.lsp")
(LOAD "backend.fas")
(DELETE-FILE "backend.fas")
(DELETE-FILE "backend.lib")

(MAPC 'clisp-install '("toplevel.kl" "core.kl" "sys.kl" "sequent.kl" "yacc.kl" "reader.kl" "prolog.kl" "track.kl" "load.kl" "writer.kl" "macros.kl" "declarations.kl" "types.kl" 
"t-star.kl"))

(COMPILE-FILE "overwrite.lsp")
(LOAD "overwrite.fas")
(DELETE-FILE "overwrite.fas")
(DELETE-FILE "overwrite.lib")

(MAPC 'FMAKUNBOUND '(boot writefile openfile))

(EXT:SAVEINITMEM "Shen.mem" :INIT-FUNCTION 'shen.byteloop)

(QUIT)