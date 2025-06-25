#lang racket

(provide (except-out (all-defined-out) SQL-QUERY-STRING))
(require "state.rkt"
         racket/gui
         plot
         db)

(define plot-details (make-hash))

(define SQL-QUERY-STRING
  #<<sql-code
select value from (select ndx, value from counts order by ndx desc limit 250) order by ndx asc
sql-code
  )

(define save-frame%
  (class frame%

    (define/override (on-size w h)
      (printf "Resized~n")
      (hash-set! plot-details
                 'bitmap
                 (new bitmap-dc% [bitmap
                                  (make-object bitmap% (inexact->exact (floor (* w 0.9)))
                                    (inexact->exact (floor (* h 0.85))))])))
    (super-new)))

(define save-canvas%
  (class canvas%

    (define/override (on-event mouse-evt)
      (when (symbol=? (send mouse-evt get-event-type) 'right-down)
        (send (send (hash-ref plot-details 'bitmap) get-bitmap)
              save-file
              (format "~a-~a.png"
                      (hash-ref plot-details 'filename)
                      (ticker))
              'png)))
    (super-new)))
      
;; FIX I'm pretty sure there's an builtin that does this

(define (intersperse ls separator)
  (cond
    [(empty? ls) empty]
    [(empty? (rest ls)) ls]
    [else
     (cons (first ls)
           (cons separator (intersperse (rest ls) separator)))]))

;; http://tools.medialab.sciences-po.fr/iwanthue/
(define distinct-colors
  (map (λ (ls)
         (make-object color% (first ls) (second ls) (third ls)))
       '([221 148 51] [100 99 211] [138 188 57] [176 95 211] 
                      [85 199 95] [204 77 172] [66 147 45] [225 74 134] 
                      [88 171 106] [206 62 70] [92 205 169] [211 89 45] 
                      [85 136 229] [192 180 55] [138 82 156] [109 135 42] 
                      [149 153 222] [175 145 62] [83 104 167] [164 182 107] 
                      [167 62 102] [52 118 67] [215 136 192] [59 158 139] 
                      [209 113 111] [72 177 218] [153 92 44] [139 74 95] 
                      [107 108 44] [223 155 110])))

(define-syntax-rule (create-plot db-tags ...)
  (let ([W 800]
        [H 600])
    (hash-set! plot-details
               'filename
               (apply string-append
                      (intersperse
                       (map (λ (o)
                              (format "~a" o))
                            (quote (db-tags ...)))
                       "_")))
    (define f (new save-frame%
                   [label "plot"]
                   [width W]
                   [height H]))

    (define c (new save-canvas% [parent f]))

    (define plot-thread
      (thread (λ ()
                (send f show true)
                (define dc (send c get-dc))
                (hash-set! plot-details
                           'bitmap
                           (new bitmap-dc%
                                [bitmap (make-object bitmap% W H)]))
                (define conns (for/list ([tag (quote (db-tags ...))])
                                (hash-ref log-conns tag)))
                (define val-max (make-parameter -10000))
                (define val-min (make-parameter  10000))
                (define data-exists? false)
                
                (let loop ()
                  (sleep 0.1)
                  
                  (define the-lines
                    (for/list ([conn conns]
                               [color-ndx  (range (length conns))])
                      
                      (define rows (query-list conn SQL-QUERY-STRING))
                      
                      (define the-data
                        (cond
                          [rows
                           ;; (printf "Plotting ~a rows~n" (length rows))
                           (define mx (apply max rows))
                           (when (< (val-max) mx)
                             (val-max mx))
                           
                           (define mn (apply min rows))
                           (when (> (val-min) mn)
                             (val-min mn))
                           
                           (define data
                             (for/vector ([value rows][index (in-naturals)])
                               (vector index value)))
                           (set! data-exists? true)
                           data]
                          [else '()]))
                      
                      (lines the-data 
                             #:color (list-ref distinct-colors
                                               (modulo color-ndx (length distinct-colors))))))
                  ;; (printf "LINES: ~a~n" the-lines)
                  (when data-exists?
                    (plot/dc the-lines
                             (hash-ref plot-details 'bitmap)
                             0 0
                             (* 0.9 (send f get-width))
                             (* 0.85 (send f get-height))
                             #:x-label "Ticks"
                             #:y-label "Turtles"
                             #:y-max (* (val-max) 1.1)
                             #:y-min (* (val-min) 1.1))
                    (send dc draw-bitmap (send (hash-ref plot-details 'bitmap) get-bitmap) 0 0))
                  'pass
                  (loop)))))
    (thread (thunk (sync (thread-dead-evt plot-thread))
                   (send f show #f)))
    (add-thread-to-kill! plot-thread)))
