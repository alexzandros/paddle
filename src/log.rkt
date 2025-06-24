#lang racket
(provide create-log log get-log log-conns)

(require db sql
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
  id integer,
  pid integer,
  x integer,
  y integer);
sql-code
  )

(define SQL-TABLE-INSERT
  #<<sql-code
insert into counts (tick, value, id, pid, x, y) values ($1, $2, $3, $4, $5, $6);
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

(define-syntax-rule (log tag agent-set)
  (let ([agent-count 0])
    (for ([agent (get-agents agent-set)])
      (parameterize ([current-agent agent])
        (when agent
          (set! agent-count (add1 agent-count))
          (query-exec
           (hash-ref log-conns (quote tag))
           SQL-TABLE-INSERT
           (ticker)
           agent-count
           (get agent agent-id)
           (get agent agent-pid)
           (get agent agent-x)
           (get agent agent-y)))))))

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