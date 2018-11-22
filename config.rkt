#lang racket/base

(provide path-prefix)

(require racket/match)

(define path-prefix
  (match (getenv "ENVIRON")
    ["production" "https://homes.cs.washington.edu/~sorawee/"]
    [_ "http://localhost:8080/"]))
