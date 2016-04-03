#lang racket
(define entryList '((((Reference_Table Reference_Column) Referenced_Table)
                     ((Car Owner) Person))
                    (Table_Name (Table_Format)(Table_Registers))
                    (Person (ID Name Age) (7115689 Malcolm 21) (1468987 Arturo 22))
                    (Car (Licence_Plate Owner Brand) (648054 7115689 Nissan) (ACS895 1468987 Audi))
                    (Per1son (ID Nombre Age) (7115689 Malcolm 21) (1468987 Arturo 22))))
(define badEntry1 '((((Reference_Table Reference_Column)() Referenced_Table)
                     ((Car Owner) Person()))()
                                            (Table_Name (Table_For()mat)(Table_Registers))
                                            (Person (ID Name Age)() (7()115689 Malcolm 21) (1468987 Arturo 22))
                                            (Car (Licence_Plate Own()er Brand) (648054 7115689 Nissan) (ACS895 1468987 Audi))
                                            (Per1son (ID Nombre Age) ()(7115689 Malcolm 21) ()(1468987 Arturo 22))))
(define badEntry2 'a)

;Method to add reference to the DB
(define (addReference DB userEntry)
  (cond
    [(not (eq? (length userEntry) 3)) write "The user entry is not valid."]
    [(eq? DB null) write "The database is empty."]
    [(not (tableExist? DB (car userEntry) 0)) write "The table doesn't exist."]
    [(not (tableExist? DB (caddr userEntry) 0)) write "The source table doesn't exist."]
    [(not (columnExist? DB (car userEntry) (cadr userEntry) 0)) write "The column doesn't exist."]
    ;Other verifications
    [#t (cons (cons (cons (car userEntry) (cadr userEntry)) (caddr userEntry)) (car DB))]
    )
  )

;Method that checks if the table exist
(define (tableExist? DB tableName level)
  (cond
    [(eq? DB null) #f]
    [(eq? DB tableName) #t]
    [(atom? DB) #f]
    [(not (eq? level 2)) (or (tableExist? (car DB) tableName (+ level 1)) (tableExist? (cdr DB) tableName level))]
    [#t #f])
  )

;Method that checks if the table is empty
(define (empty? tableName level)
  (cond
    [(eq? DB null) #f]
    [(atom? DB) #f]
    [(eq? (car DB) null) #f]
    [(eq? (car DB) tableName) (cond
                                {(null? (cdr DB)) #f}
                                {(atom? (cdr DB)) #f}
                                {#t (> (length (cdr DB)) 1)})]
    [(not (eq? level 2)) (or (columnExist? (car DB) tableName columnName (+ level 1)) (columnExist? (cdr DB) tableName columnName level))]
    [#t #f]
    ))


;Method that checks if the column exist
(define (columnExist? DB tableName columnName level)
  (cond
    [(eq? DB null) #f]
    [(atom? DB) #f]
    [(eq? (car DB) null) #f]
    [(eq? (car DB) tableName) (cond
                                {(null? (cdr DB)) #f}
                                {(atom? (cdr DB)) #f}
                                {(not (null? (cadr DB)))(contains? (cadr DB) columnName)}
                                {#t #f})]
    [(not (eq? level 2)) (or (columnExist? (car DB) tableName columnName (+ level 1)) (columnExist? (cdr DB) tableName columnName level))]
    [#t #f]
    ))

;Method that check if an element is in the list
(define (contains? list word)
  (cond
    [(eq? list null) #f]
    [(atom? list) (eq? word list)]
    [#t (or 
         (contains? (car list) word) 
         (contains? (cdr list) word))]))

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