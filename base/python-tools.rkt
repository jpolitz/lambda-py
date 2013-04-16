#lang racket/base

(require racket/pretty
         racket/fasl
         racket/list
         "get-structured-python.rkt"
         "python-interp.rkt"
         "python-phases.rkt"
         "python-phase2.rkt"
         "python-phase1.rkt"
         "python-desugar.rkt"
         "python-cps.rkt"
         "python-macros.rkt"
         "python-lib.rkt"
         (only-in "core-to-sexp.rkt" snapshot->sexp)
         (only-in "sexp-to-core.rkt" sexp->snapshot)
         (only-in "python-core-syntax.rkt" snapshot? snapshot-env snapshot-sto)
         "run-tests.rkt"
         "util.rkt"
         "python-evaluator.rkt"
         "parser/parser.rkt"
         "parser/python-lexer.rkt"
         "parser/test-parser.rkt")

(provide (all-defined-out)
         set-pypath
         get-pypath)

(define (python-test-runner _ port)
  (run-python port))

(define snapshot #f)
(define (set-snapshot-from-file filename)
  (define snap-file (open-input-file filename))
  (set! snapshot (sexp->snapshot (fasl->s-exp snap-file)))
  (close-input-port snap-file))

(define (run-python port)
  (define interpreter
    (cond
      [snapshot
       (define env (snapshot-env snapshot))
       (define sto (snapshot-sto snapshot))
       (lambda (expr) (interp-env expr env sto empty))]
      [else
       interp]))
  (define lib-wrapper
    (cond
      [snapshot (lambda (x) x)]
      [else python-lib])) 
  (interpreter
   (lib-wrapper
    (desugar-generators
     (desugar
      (new-scope-phase
       (get-structured-python
        ((parser) port))))))))

(define (run-python/snapshot port snap)
  (define snap-file (open-output-file snap #:exists 'replace))
  (define (handle-snapshot snapshot)
    (s-exp->fasl (snapshot->sexp snapshot) snap-file)
    (close-output-port snap-file))
  (with-handlers
    ([snapshot? handle-snapshot])
    (run-python port)))

(define (get-surface-syntax port)
  (get-structured-python
   ((parser) port)))

(define (get-lexical-syntax port)
  (phase2-without-locals (scope-phase
                          (get-structured-python
                           ((parser) port)))))

(define (get-phase1-syntax port)
  (scope-phase
   (get-structured-python
    ((parser) port))))

(define (get-lexical-syntax-with-locals port)
  (new-scope-phase
   (get-structured-python
    ((parser) port))))

(define (desugar-w/lex port)
  (desugar
   (new-scope-phase
    (get-structured-python
     ((parser) port)))))

(define (desugar-w/lib port)
  (python-lib
   (desugar
    (new-scope-phase
     (get-structured-python
      ((parser) port))))))

(define (desugar-w/macros port)
  (desugar
   (new-scope-phase
    (get-structured-python
     ((parser) port)))))

(define (get-core-syntax port)
  (desugar
   (new-scope-phase
    (get-structured-python
     ((parser) port)))))

(define (get-lexer-tokens port)
  (lex-all port))
