#lang racket
(require math/number-theory)
;********************************************************************************
;Main function that starts the DataBase
(define (init)
  (display "Welcome to RELDB, please entry your command")
  (newline)
  (dbManagement (list '()) )
  )

;********************************************************************************
;Manage the DataBase
(define (dbManagement DB)
  (auxManegement DB (readEntry))
  )

;********************************************************************************
;Auxiliar function for the management of DataBase
(define (auxManegement DB userEntry)
  (cond
    [(or(equal? (car userEntry) "exit") (equal? (car userEntry) "Exit")) (display "Leaving the DataBase")]
    [#t (dbManagement (interpretEntry DB userEntry))]
    )
  )

;********************************************************************************
;Function that reads the User entries
(define (readEntry)
  (newline)
  (regexp-match* #rx"(([a-zA-Z0-9]+)|(\".*\")|(\\(.*\\)))" (read-line (current-input-port) ) )
  )

;**********************************************************************************
;Method that interpreat the entry
(define (interpretEntry DB userEntry)
  (cond
    [(or (equal? (car userEntry) "addtable") (equal? (car userEntry) "addt")) (add_Table DB (cdr userEntry) ) ]
    [(equal? (car userEntry) "showall") (show (cdr DB)) DB]
    [(or (equal? (car userEntry) "insert") (equal? (car userEntry) "ins")) (mngIns DB (cadr userEntry) (cddr userEntry))]
    [(or (equal? (car userEntry) "addr")(equal? (car userEntry) "addReference")) (addReference DB (cdr userEntry))]
    [(or (equal? (car userEntry) "remr") (equal? (car userEntry) "addReference")) (rmReference DB (cdr userEntry))]
    [(equal? (car userEntry) "query")  (query DB (cdr userEntry))]
    [(equal? (car userEntry) "update")  (update DB (cdr userEntry))]
    [(or (equal? (car userEntry) "dt") (equal? (car userEntry) "deltable")) (rmRegister DB (cdr userEntry))]
    [#t (display "Error: Please entry a valid command") DB]
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNTIONS OF COMMANDS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


;********************************************************************************
;Function that create new table in DB
(define (add_Table DB userEntry)
  (cond
    [ (>= (length userEntry) 2)
      (cond
        [(and (not(null? (car userEntry))) (not(tableExist? DB (car userEntry))))  (append DB (list (cons (car userEntry) (list (cdr userEntry) ) ) ) ) ]
        [#t (display "Table already exist, use the command insert, to insert registers in the table") DB]
        )
      ]
    [#t (display "Error: Private key not specified") DB]
    )
  )

;********************************************************************************
;Method that manage the register insertions
(define (mngIns DB tableName data)
  (cond
    [(and (not(equal? data null)) (tableExist? DB tableName))
     (cond
       [(reference? (car DB) tableName)
        (cond
          [(and (not (pkeyExist?  (cddr (searchTable DB tableName)) (car data)))
                (pkeyExist? (cddr (searchTable DB (searchReferenced (car DB) tableName)))
                            (car(get data (colToNum (cadr (searchTable DB tableName)) (searchReferencedColum (car DB) tableName) ) 0))))
           (cons (car DB)(insert_data (cdr DB) tableName data))]
          [(pkeyExist?  (cddr (searchTable DB tableName)) (car data))
           (display "Error: The primary key already exists.") DB]
          [#t (display "Error: The foreing key doesn't exist, please first inser that value.") DB]
          )
        ]
       [(not (reference? (car DB) tableName))
        (cond
          [(not (pkeyExist?  (cddr (searchTable DB tableName))  (car data) ) )
           (cond
             [(list? (car data) ) (insert_by_Parameter DB tableName data)]
             [(atom? (car data) ) (cons (car DB) (insert_data (cdr DB) tableName data))]
             )
           ]
          [#t (display "Error: Primary Key Already Exists") DB]
          )
        ]
       )
     ]
    [#t (display "Error: Table doesn't exist or syntax of given records add incorrect" ) DB]
    )
  )

;************************************************************************
;Method that remove a reference on the DB
(define (rmRegister DB userEntry)
  (cond
    [(not (equal? (length userEntry) 1)) (display "The user entry is not valid.") DB]
    [(equal? DB null) (display "The database is empty.") DB]
    [(not (tableExist? DB (car userEntry))) ( display "The table doesn't exist.") DB]
    [(referenced? (car DB) (car userEntry)) ( display "The table is referenced, remove the referece.") DB]
    [#t (cons (car DB) (rmRegAux (cdr DB) (car userEntry)) )]
    )
  )


;***************************************************************************************
;Method that helps the rmReg finding the table and the positions
(define (rmRegAux DB name)
  (cond
    [(equal? DB null) null]
    [(atom? DB) null]
    [(not (list? (car DB))) null]
    [(or (null? (car DB))(atom? (car DB))) (cons (car DB) (rmRegAux (cdr DB) name))]
    [(equal? (caar DB) name)  (cdr DB)]
    [#t (cons (car DB) (rmRegAux (cdr DB) name))]
    )
  )



;*****************************************************************************
;Method to add reference to the DB
(define (addReference DB userEntry)
  (cond
    [(not (equal? (length userEntry) 3)) (display "The user entry is not valid.") DB]
    [(equal? DB null) (display "The database is empty.") DB]
    [(not (tableExist? DB (car userEntry))) ( display "The table doesn't exist.") DB]
    [(not (empty? DB (car userEntry) 0)) ( display "The table isn't empty.") DB]
    [(not (tableExist? DB (caddr userEntry))) (display "The source table doesn't exist.") DB]
    [(not (columnExist? DB (car userEntry) (cadr userEntry) 0)) (display "The column doesn't exist.") DB]
    [(reference? (car DB) (car userEntry)) (display "The table already have a reference") DB]
    [#t (cons (cons (list (list (car userEntry) (cadr userEntry)) (caddr userEntry)) (car DB)) (cdr DB))]
    )
  )

;***************************************************************************************
;Method to remove a reference from the Database
(define (rmReference DB userEntry)
  (cond
    [(not (equal? (length userEntry) 3)) (display "The user entry is not valid.") DB]
    [(equal? DB null) (display "The database is empty.") DB]
    [(not (tableExist? DB (car userEntry))) ( display "The table doesn't exist.") DB]
    [(not (empty? DB (car userEntry) 0)) ( display "The table isn't empty.") DB]
    [(not (tableExist? DB (caddr userEntry)))( display "The source table doesn't exist.") DB]
    [(not (columnExist? DB (car userEntry) (cadr userEntry) 0))( display "The column doesn't exist.") DB]
    [(not (refExist? (car DB) (car userEntry) (cadr userEntry) (caddr userEntry))) ( display "The reference doesn't exist.") DB]
    [#t (list (rmRefAux (car DB) (car userEntry) (cadr userEntry) (caddr userEntry)) (cdr DB))]
    )
  )
;***************************************************************************************
;Method that updates the records of the DB
(define (update DB userEntry)
  (cond
    [(null? DB) (display "The Database is empty.") DB]
    [(atom? userEntry) (display "Not valid entry") DB]
    [(or(not (equal? (with-modulus 2(mod (length userEntry))) 0)) (not (>= (length userEntry) 4))) (display "Invalid entry.") DB]
    [(not (tableExist? DB (car userEntry)))  (display "The table doesn't exist.") DB]
    [(not (pkeyExist? (cdr (searchTable (cdr DB) (car userEntry))) (cadr userEntry))) (display "The Primary key doesn't exist.")]
    [#t (updateAux DB (car userEntry) (cadr userEntry) (cddr userEntry))]
    )
  )

;********************************************************************************************
;Method that handles the Queries
(define (query DB userEntry)
  (cond
    [(null? userEntry) (display "Invalid Entry.") DB]
    [(atom? userEntry) (display "Invalid Entry.") DB]
    [(not(tableExist? DB (car userEntry))) (display "The table doesn't exist.") DB]
    [(equal? (length userEntry) 1) (show (searchTable DB (car userEntry))) DB]
    [{and (string-contains? (cadr userEntry) "(")
          (or (equal?  2  (length (cddr userEntry))) (equal?  0  (length (cddr userEntry))))
          (not (null?  (searchTable DB (car userEntry))))}
     (show (cons (car userEntry)
                 (cons (get
                        (cadr (searchTable DB (car userEntry)))
                        (cond
                          [(and(not (null? (string-split (string-replace (string-replace (cadr userEntry) "(" "") ")" ""))))
                               (equal? (car (string-split (string-replace (string-replace (cadr userEntry) "(" "") ")" ""))) "all"))
                           (build-list (length (cadr (searchTable DB (car userEntry)))) values)]
                          [#t (colToNum (cadr (searchTable DB (car userEntry)))
                                        (string-split (string-replace (string-replace (cadr userEntry) "(" "") ")" "")))])
                        0)
                       {queryAux (cddr (searchTable DB (car userEntry)))
                                 (cond
                                   [(and(not (null? (string-split (string-replace (string-replace (cadr userEntry) "(" "") ")" ""))))
                                        (equal? (car (string-split (string-replace (string-replace (cadr userEntry) "(" "") ")" ""))) "all"))
                                    (build-list (length (cadr (searchTable DB (car userEntry)))) values)]
                                   [#t (colToNum (cadr (searchTable DB (car userEntry)))
                                                 (string-split (string-replace (string-replace (cadr userEntry) "(" "") ")" "")))])
                                 (cons (colToNum
                                        (cadr (searchTable DB (car userEntry)))
                                        (cond
                                          [(equal?  0  (length (cddr userEntry))) null]
                                          [#t (caddr userEntry)]))
                                       (cond
                                         [(equal?  0  (length (cddr userEntry))) null]
                                         [#t (cdddr userEntry)]))
                                 }
                       )
                 )
           )
     DB]
    [#t (display "Invalid Entry.") DB]
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; OTHERS FUNC ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;********************************************************************************
;Method thas checks if the parameter is an atom  
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

;********************************************************************************
;Search if a element exist on the list
(define (searchAux list name actualLevel searchLevel)
  (cond
    [(null? list) #f]
    [(and (not (equal? actualLevel searchLevel))(not (atom? list))) (or
                                                                     (searchAux (car list) name (+ actualLevel 1) searchLevel)
                                                                     (searchAux (cdr list) name actualLevel searchLevel))]
    
    [(equal? actualLevel searchLevel) (equal? list name)]
    [#t #f])
  )

;********************************************************************************
;Method that search if a PK already exist
(define (searchReferenced references tableName)
  (cond
    [(atom? references) null]
    [(null? references) null]
    [(atom? (car references)) null (display "1")]
    [(null? (car references)) null (display "2")]
    [(atom? (cdr references)) null (display "3")]
    ;[(null? (cdr references)) null (display "4")]
    [(equal? (caaar references) tableName) (car(cdr(car references)))]
    [#t (searchReferenced (cdr references) tableName)]
    )
  )

;********************************************************************************
;Method that checks if a table has been referenced
(define (reference? references sourceTable)
  (searchAux references sourceTable 0 3)
  )

;********************************************************************************
;Method that checks if a table has been referenced
    (define (referenced? references sourceTable)
      (searchAux references sourceTable 0 2)
      )

;********************************************************************************
;Auxiliar for search registers in a table
(define (containsReg? table regName)
  (cond
    [(null? table) #f]
    [(atom? table) #f]
    [(null? (car table)) #f]
    [(atom? (car table)) #f]
    [(not (atom? (caar table))) #f]
    [(string-locale=? (caar table) regName) #t]
    [#t (containsReg? (cdr table) regName)]
    )
  )

;********************************************************************************
;Method that check if a pKey is in a table
(define (pkeyExist? registers pkey)
  (cond
    [(null? registers) #f]
    [(atom? registers) #f]
    [#t (or(containsIn? (car registers) (list 0 pkey) 0)( pkeyExist? (cdr registers) pkey))]
    )
  )


;********************************************************************************
;Method that check if a register is in a table
(define (registerExist? DB tableName regName)
  (cond
    [(equal? DB null) #f]
    ;[(null? (caar DB)) #f]
    ;[(atom? (caar DB)) #f]
    [(equal? (caar DB) tableName) (cond
                                    [(containsReg? (caar (cddr (car DB))) regName) #t]
                                    [#t (registerExist? (cdr DB) tableName regName)]
                                    )
                                  ]
    [(registerExist? (cdr DB) tableName regName)]
    [#t #f]
    )
  )

;********************************************************************************
;Method that checks if the table exist
(define (tableExist? DB tableName)
  (cond
    [(not(null? tableName))
     (searchAux DB tableName 0 2)
     ]
    [#t (display "Error: Please enter a valid entry")]
    )
  )

;***********************************************************************************
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
    [(and (equal? (caar references) table)(equal? (cadar references) column)(equal? (cadr references) sourceTable)) #t]
    [#t (or (refExist? (car references) table column sourceTable)(refExist? (cdr references) table column sourceTable))]
    )
  )

;*********************************************************************************
;Method that checks if the table is empty
(define (empty? DB tableName level)
  (cond
    [(equal? DB null) #f]
    [(atom? DB) #f]
    [(equal? (car DB) tableName) (cond
                                   {(null? (cdr DB)) #f}
                                   {(atom? (cdr DB)) #f}
                                   {#t (< (length (cdr DB)) 2)})]
    [(not (equal? level 2)) (or (empty? (car DB) tableName  (+ level 1)) (empty? (cdr DB) tableName  level))]
    [#t #f]
    ))


;***********************************************************************************
;Method that checks if the column exist
(define (columnExist? DB tableName columnName level)
  (cond
    [(equal? DB null) #f]
    [(atom? DB) #f]
    ;[(equal? (car DB) null) #f]
    [(equal? (car DB) tableName) (cond
                                   {(null? (cdr DB)) #f}
                                   {(atom? (cdr DB)) #f}
                                   {(not (null? (cadr DB)))(contains? (cadr DB) columnName)}
                                   {#t #f})]
    [(not (equal? level 2)) (or (columnExist? (car DB) tableName columnName (+ level 1)) (columnExist? (cdr DB) tableName columnName level))]
    [#t #f]
    ))

;Auxiliar method to update the Database records
(define (updateAux DB tableName PK userEntry)
  (cond
    [(null? DB) DB]
    [(atom? DB) DB]
    [(null? (car DB)) DB]
    [(atom? (car DB)) DB]
    [(not (equal? (caar DB) tableName)) (cons (car DB) (updateAux (cdr DB) tableName PK userEntry) )]
    [#t (cons(append (append
                      (cons (caar DB) '())
                      (cons(cadar DB) '()))
                     (updateAuxAux (cddar DB) (cadar DB) PK userEntry)) (cdr DB))]
    )
  )

;************************************************************************************
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
    [(not (equal? (caar table) PK))(cons (car table)(updateAuxAux (cdr table) columns PK userEntry))]
    [(equal? (position columns (car userEntry) 0) 0 ) (display "The Primary Key cannot be changed.") table] 
    [#t (updateAuxAux (cons (
                             replace (car table) (cadr userEntry) 0 (position columns (car userEntry) 0))
                            (cdr table))
                      columns PK (cddr userEntry))]
    )
  )

;************************************************************************************
;Method that replace a value on the list
(define (replace list value actualPos pos)
  (cond
    [(null? list) list]
    [(and (not (atom? list)) (not (equal? actualPos pos))) (cons (car list) (replace (cdr list) value (+ actualPos 1) pos))]
    [(equal? actualPos pos ) (cons value (cdr list))]
    [#t list]
    ))

;************************************************************************************
;Method that returns a representation of the position of the column with a int
(define (position columns name pos)
  (cond
    [(or (atom? columns)(null? columns)) -1]
    [(equal? (car columns) name) pos]
    [#t (position (cdr columns) name (+ 1 pos))]
    ))

;************************************************************************************
;Method that check if an element is in the list
(define (contains? list word)
  (cond
    [(equal? list null) #f]
    [(atom? list) (equal? word list)]
    [#t (or 
         (contains? (car list) word) 
         (contains? (cdr list) word))])
  )



;********************************************************************************
;Method that inserts registers by paramters
(define(insert_data_by_Parameter DB tableName data)
  DB)

;********************************************************************************
;Method that inserts data in the table, if the table exist
(define (insert_data DB tableName data)
  (cond
    [(null? DB) null]
    [(pkeyExist?  (cdr DB) (car data)) (display "Error: Null data or register (PK) already exist") DB] 
    [(and (not (equal? DB null)) (not (equal? data null)) (>= (length data) 1))
     (cond
       [(atom? (car DB))
        (cond
          [(equal? (car DB) tableName) (auxIns DB tableName data)]
          [(not(equal? (car DB) tableName)) DB]
          )]
       [#t(cons (insert_data (car DB) tableName data) (insert_data (cdr DB) tableName data))])
     ]
    [(and (equal? data null) (pkeyExist?  (cddr (searchTable DB tableName)) (car data)))
     (display "Error: Null data or register (PK) already exist") DB
     ]
    )
  )

;********************************************************************************
;auxiliar function for insert registers
(define (auxIns DB tableName data)
  (cond
    [(equal? (length (car (cdr DB))) (length data)) (cons (car DB) (append (cdr DB) (list data)))]
    [(> (length (car (cdr DB))) (length data)) (auxIns DB tableName (append data (list "nil")))]
    [(< (length (car (cdr DB))) (length data)) (display "Impossible to insert registers, more registers than necessary, please verify") DB]))
;************************************************************************************
;Auxiliar method of the rmReference method
(define (rmRefAux references table column sourceTable)
  (cond
    [(atom? references) null]
    [(null? references) null]
    [(atom? (car references)) null]
    [(null? (car references)) null]
    [(atom? (cdr references)) null]
    [(equal? (caaar references) table) (rmRefAux (cdr references) table column sourceTable)]
    [#t (cons(car references) (rmRefAux (cdr references) table column sourceTable) )]
    )
  )

;***************************************************************************************
;Method that helps the query finding the table and the positions
(define (searchTable DB name)
  (cond
    [(equal? DB null) null]
    [(atom? DB) null]
    [(not (list? (car DB))) null]
    [(null? (car DB)) (searchTable (cdr DB) name)]
    [(equal? (caar DB) name) (car DB)]
    [#t (searchTable (cdr DB) name)]
    )
  )


;**********************************************************************************************
;Auxiliar method to handle the queries
(define (queryAux registers columNumbers condition)
  (cond
    [(contains? columNumbers -1) (display "The Query Column doesn't exist.") null]
    [(atom? condition) (display "Invalid Condition Entry") null]
    [(null? registers) null]
    [(or (null? condition)(null? (car condition))(containsIn? (car registers) condition 0))
     (cons (get (car registers) columNumbers 0) (queryAux (cdr registers) columNumbers condition))]
    [(equal? (car condition) -1) (display "The Condidion Column doesn't exist.") null]
    [#t (queryAux (cdr registers) columNumbers condition)]
    )
  )

;**********************************************************************************************
;Method that transforms the columns into numbers
(define (colToNum columns columnNames)
  (cond
    [(null? columnNames) null]
    [(atom? columnNames) (position columns  columnNames 0)] 
    [#t (cons (position columns (car columnNames) 0) (colToNum columns (cdr columnNames)))]
    )
  )

;**********************************************************************************************
;Method that gets the data from a column in a register
(define (get register columNumbers actualNumber)
  (cond
    [(null? register) null]
    [(contains? columNumbers actualNumber) (cons (car register) (get (cdr register) columNumbers (+ actualNumber 1)))]
    [#t (get (cdr register) columNumbers (+ actualNumber 1))]
    )
  )

;**********************************************************************************************
;Method that checks if the register pass the condition 
(define (containsIn? register condition actualColumn)
  (cond
    [(not(equal? (length condition) 2)) #f]
    [(null? register) #f]
    [(null? condition) #t]
    [(and (atom? register) (not (equal? actualColumn (car condition))))#f]
    [(and (equal? actualColumn (car condition)) (equal? (cadr condition) (car register))) #t]
    [#t (containsIn? (cdr register) condition (+ 1 actualColumn))]
    )
  )

;**********************************************************************************************
;Method that Formats and show the Database or the query in a user friendly manner
(define (show DB)
  (cond
    [(null? DB) (display "")]
    [(atom? DB) (display "")]
    [(null? (car DB)) (display "")]
    [(and (atom? (car DB)) (not (null? (cdr DB))) (list? (cadr DB))  (> (length (cadr DB)) 0))
     (showAux  DB 0 #t (length (cadr DB)))(newline)(display (make-string 132 #\-))(newline)]
    [(or(null? (car DB)) (null? (caar DB))) (display "")]
    [(and (atom? (caar DB)) (not (null? (cdar DB))) (list? (cadar DB) ))
     (showAux (car DB) 0 #t (length (cadar DB) ))(newline)(display (make-string 132 #\-))(newline) (show (cdr DB))]
    [#t (show (cdr DB))]
    )
  )

;**********************************************************************************************
;Auxiliar method to show the table in a User Friendly manner
(define (showAux table level change columns)
  (cond
    [change (newline)(display (make-string 132 #\-))(newline)(showAux table level #f columns)]
    [(null? table) (display "")]
    [(atom? table) (display (format table (- (quotient  130  (* columns 2))(string-length table))))
                   (cond [(equal? 1 columns)(newline)(display (make-string 132 #\-))(newline)]
                         [#t (display "")]
                         )]
    [(equal? level 0) (display (format (car table) (quotient (- 130 (string-length (car table))) 2) ))
                      (showAux (cdr table) (+ level 1) #t columns)]
    [(and (equal? (with-modulus columns (mod (- level 1))) 0) (not (equal? 1 columns)))
     (showAux (car table) (+ level 1) #t columns) (showAux (cdr table) level #f columns)]
    [#t (showAux (car table) level #f columns) (showAux (cdr table) level #f columns)]
    ))

;**********************************************************************************************
;Method that format a string to show it
(define (format string space)
  (cond [(> space 0) (string-append "|" (make-string space #\ ) string (make-string space #\ ) "|")]
        [#t (string-append "|" (make-string 1 #\ ) string (make-string 1 #\ ) "|")])
  )

;********************************************************************************
;Method that search a table referenced from another table
(define (searchReferencedColum references tableName)
  (cond
    [(atom? references) null]
    [(null? references) null]
    [(atom? (car references)) null ]
    [(null? (car references)) null ]
    [(atom? (cdr references)) null ]
    [(equal? (caaar references) tableName) (cadaar references)]
    [#t (searchReferencedColum (cdr references) tableName)]
    )
  )

;********************************************************************************
;Method that inserts registers by paramters
(define(insert_by_Parameter DB tableName data)
  DB)


;*****************************************************************************
(define (entryForeingKey data pos init)
  (cond
    [(not (null? data)) null]
    [(> pos (length data)) (display "Error: Foreing Key not given")]
    [(equal? pos init) (car data)]
    [#t (entryForeingKey (cdr data) pos (+ init 1))]
    )
  )

;************************************************************************
(define (parameters tables tableName)
  (cond
    [(null? tables) null]
    [(equal? (caar tables) tableName)
     (cdar tables)]
    [#t (parameters (cdr tables) tableName)]
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENDS OF THE PROGRAM ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(init)
(define entryList '(((("Reference_Table" "Reference_Column") "Referenced_Table")
                     (("Car" "Owner") "Person")(("table" "column") "originalTable"))
                    ("Table_Name" ("Table_Format")("Table_Registers"))
                    ("Person" ("ID" "Name" "Age") ("7115689" "Malcolm" "21")("65465" "Juan" "21") ("646465" "Santi" "21") ("1468987" "Arturo" "22"))
                    ("Car" ("License_Plate" "Owner" "Brand") ("648054" "7115689" "Nissan") ("ACS895" "1468987" "Audi"))
                    ("Mechanic" ("ID" "Data" "Car"))))
;(pkeyExist? (cddr (searchTable entryList "Person")) "7115689")
;(searchReferencedColum (car entryList) "tabe")
(rmRegister entryList '("Car" "648054"))