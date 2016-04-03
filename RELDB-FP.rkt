#lang racket
(define entryList '((((Reference_Table Reference_Column) Referenced_Table)
                     ((Car Owner) Person))
                    (Table_Name (Table_Format)(Table_Registers))
                    (Person (ID Nombre Age) (7115689 Malcolm 21) (1468987 Arturo 22))
                    (Car (Licence_Plate Owner Brand) (648054 7115689 Nissan) (ACS895 1468987 Audi))))

;Method to add reference to the DB
(define (addReference DB userEntry)
  (cond
    [(not (eq? (length userEntry)) 3) write "The user entry is not valid."]
    [(eq? DB null) write "The database is empty"]
    ;Other verifications
    [#t write]
    )
  )

;Method that checks if the table exist
(define (tableExist? DB tableName level)
  (cond
    [(eq? DB null) #f]
    [(eq? DB tableName) #t]
    [(> level 2) #f]
    [#t (or (tableExist? (car DB (+ level 1))) (tableExist? (cdr (DB) (+ level 1))))]))

;Method to test if something is an atom
(define (atom? element)
  (and (not (null? element))
       (not (pair? element))))

(define (listmng x) 
  (cond
    [(or
      (atom? x)
      (null? x)) x]
    [#t ( cons
          (listmng (car x))
          (listmng (cdr x)))]))

(define (readBox)
  (regexp-split #rx" +" (read-line
                         (current-input-port)))
  )