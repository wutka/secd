(use utils)
(require-extension srfi-1)
(require-extension srfi-13)

(define opcodes '(("NIL" (0))
		  ("LDC" (1 INT))
		  ("LD"  (2 BYTE BYTE))
		  ("ATOM" (3))
		  ("CAR" (4))
		  ("CDR" (5))
		  ("CONS" (6))
		  ("ADD" (7))
		  ("SUB" (8))
		  ("MUL" (9))
		  ("DIV" (10))
		  ("MOD" (11))
		  ("SEL" (12 INT INT))
		  ("JOIN" (13))
		  ("LDF" (14 INT))
		  ("AP" (15 BYTE))
		  ("RTN" (16))
		  ("DUM" (17 BYTE))
		  ("RAP" (18 BYTE))
		  ("STOP" (19))
		  ("CGE" (20))
		  ("CGT" (21))
		  ("CEQ" (22))
		  ("CNE" (23))
		  ("CLE" (24))
		  ("CLT" (25))
		  ("TAP" (18 BYTE))
		  ("TSEL" (26 INT INT))
		  ))

(define (comment? l)
  (string-prefix? ";" l))

(define (not-comment? l) (not (comment? l)))

(define fileout #f)

(define (output-byte b)
  (if (< b 16)
    (format fileout "0~X"b)
    (format fileout "~X"b)))

(define (assemble-file file-in file-out)
  (set! fileout (open-output-file file-out))
<<<<<<< HEAD
  (format fileout ">>:")
=======
  (format fileout ">>")
>>>>>>> 9a34470afd3357268a5e4676c4d6c0f586db78b4
  (let ((lines (string-split (read-all file-in) "\n")))
    (map assemble (filter not-comment? lines)))
  (format fileout "<")
  (close-output-port fileout))

(define (assemble-int i)
  (display "assemble-int ")(display i)(newline)
  (output-byte (bitwise-and (arithmetic-shift i -24) 255))
  (output-byte (bitwise-and (arithmetic-shift i -16) 255))
  (output-byte (bitwise-and (arithmetic-shift i -8) 255))
  (output-byte (bitwise-and i 255)))

(define (assemble-args p i)
  (display "assemble-args ")
  (display p)
  (display " ")
  (display i)(newline)
  (if (not (null? p))
      (begin
	(display (car i))(display " ")(display (equal? (car i) 'INT))(newline)
	(cond
	 [(equal? (car i) 'BYTE) (output-byte (string->number (car p)))]
	 [(equal? (car i) 'INT) (assemble-int (string->number (car p)))])
	(assemble-args (cdr p) (cdr i)))))

(define (assemble line)
  (let* [(parts (string-split line))
	 (instr (cadr (assoc (car parts) opcodes)))]
    (display parts)(newline)
    (display instr)(newline)
    (output-byte (car instr))
    (assemble-args (cdr parts) (cdr instr))))
    
(assemble-file (cadr (argv)) (caddr (argv)))
