#lang racket/base

(provide path-prefix lang path-prefix-lang)

(require racket/match
         racket/format)

(define path-prefix
  (match (getenv "ENVIRON")
    ["production" "https://homes.cs.washington.edu/~sorawee/"]
    [_ "http://localhost:8080/"]))

(define lang (or (getenv "LANG") "en"))

(define path-prefix-lang
  (match (getenv "ENVIRON")
    ["production" (~a path-prefix lang "/")]
    [_ path-prefix]))
