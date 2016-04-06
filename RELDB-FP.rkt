#lang racket
(require math/number-theory)
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


;Method that updates the records of the DB
(define (update DB userEntry)
  (cond
    [(null? DB) null (display "The Database is empty.")]
    [(atom? userEntry) "Not valid entry"]
    [(or(not (eq? (with-modulus 2(mod (length userEntry))) 0)) (not (>= (length userEntry) 4))) null (display "Invalid entry.")]
    [(not (tableExist? DB (car userEntry))) null (display "The table doesn't exist.")]
    [(not (registerExist? DB (car userEntry) (cadr userEntry))) "The Primary key doesn't exist."]
    [#t (updateAux DB (car userEntry) (cadr userEntry) (cddr userEntry))]
    )
  )

;Auxiliar method to update the Database records
(define (updateAux DB tableName PK userEntry)
  (cond
    [(null? DB) DB]
    [(atom? DB) DB]
    [(null? (car DB)) DB]
    [(atom? (car DB)) DB]
    [(not (eq? (caar DB) tableName)) (cons (car DB) (updateAux (cdr DB) tableName PK userEntry) )]
    [#t (cons(append (append
                      (cons (caar DB) '())
                      (cons(cadar DB) '()))
                     (updateAuxAux (cddar DB) (cadar DB) PK userEntry)) (cdr DB))]
    )
  )

;Recursion auxiliar method to update the Database records
(define (updateAuxAux table columns PK userEntry)
  (cond
    [(null? userEntry) table]
    [(not (>= (length userEntry) 2)) table]
    [(null? table) table]
    [(atom? table)table]
    [(null? (car table))table]
    [(atom? (car table)) table]
    [(not (contains? columns (car userEntry))) (display "The column doesn't exist.") table]
    [(not (eq? (caar table) PK))(cons (car table)(updateAuxAux (cdr table) columns PK userEntry))]
    [(eq? (position columns (car userEntry) 0) 0 ) (display "The Primary Key cannot be changed.") table] 
    [#t (updateAuxAux (cons (
                                                              replace (car table) (cadr userEntry) 0 (position columns (car userEntry) 0))
                                                             (cdr table))
                                                       columns PK (cddr userEntry))]
    )
  )

;Method that replace a value on the list
(define (replace list value actualPos pos)
  (cond
    [(null? list) list]
    [(and (not (atom? list)) (not (eq? actualPos pos))) (cons (car list) (replace (cdr list) value (+ actualPos 1) pos))]
    [(eq? actualPos pos ) (cons value (cdr list))]
    [#t list]
    ))

;Method that returns a representation of the position of the column with a int
(define (position columns name pos)
  (cond
    [(eq? (car columns) name) pos]
    [(or (atom? columns)(null? columns)) -1]
    [#t (position (cdr columns) name (+ 1 pos))]
    ))
    
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
    (define (atom? x)
      (and (not (null? x))
           (not (pair? x))))
    
    ;Method that read the user entry
    (define (readBox)
      (regexp-split #rx"(( +\"?)|(\"? +)|)" (read-line
                                             (current-input-port)))
      )
    
    
    
    ;(addReference entryList '(Mechanic Data Person))
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (init DB)
      (add_Table DB ) )
    
    
    
    
    ;Method that add new tables to DB
    (define (add_Table DB userEntry)
      (cond
        [(not(tableExist? DB (car userEntry)))(append DB (list(cons (car userEntry) (list(cdr userEntry)))))]
        [#t (display "Table already exist, use the command insert, to insert registers in the table")]
        ))
    
    ;Method that manage the register insertions
    (define (mngIns DB tableName data)
      (cond
        [(and (not(eq? data null)) (not(tableExist? DB tableName)))
         (cond
           [(not(reference? DB tableName))
            (cond
              [(list? (car data)) (insert_data_by_Parameter DB tableName data)]
              [(atom? (car data)) (insert_data DB tableName data)]
              [#t (display "Not valid entries")])]
           [(reference? (car DB) tableName)
            (cond
              [((registerExist? DB (searchReferenced (car DB) tableName) (car data))) refExist? (car DB)])])]) 
      [#t (display "Table doesn't exist or syntax of given records were incorrect" )]
      )
    ;Method that inserts registers by paramters
    (define(insert_data_by_Parameter DB tableName data)
      DB)
    
    ;Method that inserts data in the table, if the table exist
    (define (insert_data DB tableName data)
      (cond
        [(not(eq? DB null))
         (cond
           [(atom? (car DB))
            (cond
              [(eq? (car DB) tableName) (auxIns DB tableName data)]
              [(not(eq? (car DB) tableName)) DB]
              )]
           [#t(cons (insert_data (car DB) tableName data) (insert_data (cdr DB) tableName data))]
           )]
        [#t DB])
      ) 
    
    ;auxiliar function for insert registers
    (define (auxIns DB tableName data)
      (display data)
      (display (car(cdr DB)))
      (newline)
      (cond
        [(eq? (length (car (cdr DB))) (length data)) (cons (car DB) (append (cdr DB) (list data)))]
        [(> (length (car (cdr DB))) (length data)) (auxIns DB tableName (append data (list 'nil)))]
        [(< (length (car (cdr DB))) (length data)) (display "Imposible to insert registers, more registers than necessary, please verify")]))
    
    
    ;Method that check if a register is in a table
    (define (registerExist? DB tableName regName)
      (cond
        [(eq? DB null) #f]
        [(eq? (caar DB) tableName) (cond
                                     [(containsReg? (caar (cddr (car DB))) regName) #t]
                                     [(registerExist? (cdr DB) tableName regName)]
                                     [#t #f])]
        [(registerExist? (cdr DB) tableName regName)]
        [#t #f]
        ))
    
    ;Auxiliar for search registers in a table
    (define (containsReg? pKey regName)
      (cond
        [(eq? pKey regName) #t]
        [#t #f]
        ))
    
    ;Method that search the PK in the referenced table
    (define (searchReferenced references tableName)
      (cond
        [(atom? references) null]
        [(null? references) null]
        [(atom? (car references)) null (display "1")]
        [(null? (car references)) null (display "2")]
        [(atom? (cdr references)) null (display "3")]
        ;[(null? (cdr references)) null (display "4")]
        [(eq? (caaar references) tableName) (car(cdr(car references)))]
        [#t (searchReferenced (cdr references) tableName)]
        ))
    
    
 ;(insert_data '((Insti (Nombre IdI Localizacion))(Personas(Id Nombre Apellido))) 'Insti '(TEC 123 Cartago))
 ;(registerExist? '((Insti (Nombre IdI Localizacion) (TEC 123 Cartago)) (Personas (Id Nombre Apellido))) 'Insti 'TEC)
 ;(add_Table'((Institucion (Nombre IdI Localizacion))) '(Persona IdP Nombre Apellido))
 ;(searchReferenced '(((Reference_Table Reference_Column) Referenced_Table)((Car Owner) Person)) 'Car)
 ;(addReference entryList '(Mechanic Data Person))
 ;(referenced? (car entryList) 'Person)
 ;(refExist? (car entryList) 'Car 'Owner 'Person)
 ;(rmRefAux (car entryList) 'table 'column 'originalTable)
 (update entryList '(Car 648054 Brand Audi Owner Pene))
 (update entryList '(Car 648054 Brand Audi))