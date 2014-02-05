;;SBCL Installation
;;install and wipe away the junk

(PROCLAIM '(OPTIMIZE (DEBUG 0) (SPEED 0) (SAFETY 3)))
(DECLAIM (SB-EXT:MUFFLE-CONDITIONS SB-EXT:COMPILER-NOTE)) 
(SETF SB-EXT:*MUFFLED-WARNINGS* T) 
(IN-PACKAGE :CL-USER)
(SETF (READTABLE-CASE *READTABLE*) :PRESERVE)
(SETQ *language* "Common Lisp")
(SETQ *implementation* "SBCL")
(SETQ *porters* "Mark Tarver")
(SETQ *release* "1.1.1")
(SETQ *port* 1.7)
(SETQ *porters* "Mark Tarver")
(SETQ *os* "Windows 7")

(DEFUN sbcl-install (File)
  (LET* ((Read (read-in-kl File))
         (Intermediate (FORMAT NIL "~A.intermed" File))
         (Write (write-out-kl Intermediate Read)))
        (boot Intermediate)
        (COMPILE-FILE (FORMAT NIL "~A.lsp" Intermediate))
   	(LOAD (FORMAT NIL "~A.fasl" Intermediate))
        (DELETE-FILE Intermediate)
	(move-file (FORMAT NIL "~A.lsp" Intermediate))
	(DELETE-FILE (FORMAT NIL "~A.fasl" Intermediate))
      (DELETE-FILE File)  ))

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
  (HANDLER-CASE (DELETE-FILE File)
      (ERROR (E) NIL))
  (WITH-OPEN-FILE (Out File :DIRECTION :OUTPUT :IF-EXISTS :OVERWRITE :IF-DOES-NOT-EXIST :CREATE)
   (FORMAT Out "~{~C~}" Chars)))

(COMPILE 'write-out-kl)

(COMPILE-FILE "primitives.lsp")
(LOAD "primitives.fasl")
(DELETE-FILE "primitives.fasl")  

(COMPILE-FILE "backend.lsp")
(LOAD "backend.fasl")
(DELETE-FILE "backend.fasl")

(MAPC 'sbcl-install '("toplevel.kl" "core.kl" "sys.kl" "sequent.kl" "yacc.kl"
                       "reader.kl" "prolog.kl" "track.kl" "load.kl" "writer.kl"
                       "macros.kl" "declarations.kl" "types.kl" 
                       "t-star.kl"))

(COMPILE-FILE "overwrite.lsp")
(LOAD "overwrite.fasl")
(DELETE-FILE "overwrite.fasl")

(MAPC 'FMAKUNBOUND '(boot writefile openfile))

(SAVE-LISP-AND-DIE "Shen.exe" :EXECUTABLE T :TOPLEVEL 'shen.shen)

