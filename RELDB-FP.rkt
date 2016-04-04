#lang racket
(define entryList '((((Reference_Table Reference_Column) Referenced_Table)
                     ((Car Owner) Person)((table column) originalTable))
                    (Table_Name (Table_Format)(Table_Registers))
                    (Person (ID Name Age) (7115689 Malcolm 21) (1468987 Arturo 22))
                    (Car (Licence_Plate Owner Brand) (648054 7115689 Nissan) (ACS895 1468987 Audi))
                    (Mechanic (ID Data Car))
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
    [(not (eq? (length userEntry) 3)) null (display "The user entry is not valid.")]
    [(eq? DB null) null (display "The database is empty.")]
    [(not (tableExist? DB (car userEntry))) null( display "The table doesn't exist.")]
    [(not (empty? DB (car userEntry) 0)) null( display "The table isn't empty.")]
    [(not (tableExist? DB (caddr userEntry))) null (display "The source table doesn't exist.")]
    [(not (columnExist? DB (car userEntry) (cadr userEntry) 0)) null (display "The column doesn't exist.")]
    [(reference? (car DB) (car userEntry)) null (display "The table already have a reference")]
    ;Other verifications
    [#t (cons (cons (cons (cons (car userEntry) (cadr userEntry)) (caddr userEntry)) (car DB)) (cdr DB))]
    )
  )

;Method to remove a reference from the Database
(define (rmReference DB userEntry)
  (cond
    [(not (eq? (length userEntry) 3)) null (display "The user entry is not valid.")]
    [(eq? DB null) null (display "The database is empty.")]
    [(not (tableExist? DB (car userEntry)))null ( display "The table doesn't exist.")]
    [(not (empty? DB (car userEntry) 0))null ( display "The table isn't empty.")]
    [(not (tableExist? DB (caddr userEntry)))null ( display "The source table doesn't exist.")]
    [(not (columnExist? DB (car userEntry) (cadr userEntry) 0))null ( display "The column doesn't exist.")]
    [(not (refExist? (car DB) (car userEntry) (cadr userEntry) (caddr userEntry)))null ( display "The reference doesn't exist.")]
    ;Other verifications
    [#t (rmRefAux (car DB) (car userEntry) (cadr userEntry) (caddr userEntry))]
    )
  )

;Auxiliar method of the rmReference method
(define (rmRefAux references table column sourceTable)
  (cond
    [(atom? references) null]
    [(null? references) null]
    [(atom? (car references)) null]
    [(null? (car references)) null]
    [(atom? (cdr references)) null]
    [(eq? (caaar references) table) (rmRefAux (cdr references) table column sourceTable)]
    [#t (cons(car references) (rmRefAux (cdr references) table column sourceTable) )]
    )
  )


;Method that checks if a reference exist
(define (refExist? references table column sourceTable)
  (cond
    [(atom? references) #f]
    [(null? references) #f]
    [(atom? (car references)) #f]
    [(null? (car references)) #f]
    [(atom? (cdr references)) #f]
    [(atom? (cdar references)) #f]
    [(null? (cdar references)) #f]
    [(and (eq? (caar references) table)(eq? (cadar references) column)(eq? (cadr references) sourceTable)) #t]
    [#t (or (refExist? (car references) table column sourceTable)(refExist? (cdr references) table column sourceTable))]
    )
  )


;Method that checks if a table has been referenced
(define (referenced? references sourceTable)
  (searchAux references sourceTable 0 2)
  )

;Method that checks if a table has been referenced
(define (reference? references sourceTable)
  (searchAux references sourceTable 0 3)
  )

;Method that checks if the table exist
(define (tableExist? DB tableName)
  (searchAux DB tableName 0 2)
  )

;Search if a element exist on the list
(define (searchAux list name actualLevel searchLevel)
  (cond
    [(eq? list name) #t]
    [(atom? list) #f]
    [(null? list) #f]
    [(not (eq? actualLevel searchLevel)) (or
                                          (searchAux (car list) name (+ actualLevel 1) searchLevel)
                                          (searchAux (cdr list) name actualLevel searchLevel))]
    [#t #f])
  )

;Method that checks if the table is empty
(define (empty? DB tableName level)
  (cond
    [(eq? DB null) #f]
    [(atom? DB) #f]
    [(eq? (car DB) tableName) (cond
                                {(null? (cdr DB)) #f}
                                {(atom? (cdr DB)) #f}
                                {#t (< (length (cdr DB)) 2)})]
    [(not (eq? level 2)) (or (empty? (car DB) tableName  (+ level 1)) (empty? (cdr DB) tableName  level))]
    [#t #f]
    ))


;Method that checks if the column exist
(define (columnExist? DB tableName columnName level)
  (cond
    [(eq? DB null) #f]
    [(atom? DB) #f]
    ;[(eq? (car DB) null) #f]
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

;(addReference entryList '(Mechanic Data Person))
;(referenced? (car entryList) 'Person)
;(refExist? (car entryList) 'Car 'Owner 'Person)
;(rmRefAux (car entryList) 'table 'column 'originalTable)