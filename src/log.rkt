#lang racket
(provide create-log log get-log log-conns)

(require db sql
         json racket/mutable-treelist
         "get-set.rkt"
         "netlogo.rkt"
         "agents.rkt"
         "state.rkt")

(define SQL-TABLE-CREATION
  #<<sql-code
create table counts(
  ndx integer primary key,
  tick integer,
  value integer,
  jsondata text);
sql-code
  )

(define SQL-TABLE-INSERT
  #<<sql-code
insert into counts (tick, value, jsondata) values ($1, $2, $3);
sql-code
  )

(define-syntax-rule (create-log log-tag)
  (begin
    (define fname (format "~a.sqlite" (quote log-tag)))
    ;; Make sure it is an SQLite DB file.
    (when (and (file-exists? fname)
               (regexp-match "sqlite$" fname))
      (delete-file fname))
    ;; Create an empty file.
    (close-output-port (open-output-file fname))
    
    (let ([conn (sqlite3-connect #:database fname)])
      (hash-set! log-conns (quote log-tag) conn)
      (query-exec
       conn
       SQL-TABLE-CREATION))))

(define (get-log tag)
  (hash-ref log-conns tag))

(define (agent->hash agent)
  (hash 'id (get agent agent-id)
        'pid (get agent agent-pid)
        'x (get agent agent-x)
        'y (get agent agent-y)))

(define-syntax-rule (log tag agent-set)
  (let ([agent-count 0][tick-data (mutable-treelist)])
    (for ([agent (get-agents agent-set)])
      (parameterize ([current-agent agent])
        (when agent
          (define agent-data (agent->hash agent))
          (mutable-treelist-add! tick-data agent-data)
          (set! agent-count (add1 agent-count)))))
    (query-exec (hash-ref log-conns (quote tag))
                SQL-TABLE-INSERT
                (ticker)
                agent-count
                (with-output-to-string
                  (thunk (write-json (mutable-treelist->list tick-data)))))
    agent-count))

(module+ test
  (require "netlogo.rkt"
           "agents.rkt"
           "get-set.rkt"
           "breeds.rkt"
           "state.rkt")
  
  (create-breed turtle turtles)
  (create turtles 200)
  
  (create-log turtles)

  (for ([tick (range 100)])
    (ticker tick)
    (log turtles
         (where turtles (< (get turtle-id) 20)))))