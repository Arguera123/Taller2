#lang racket

(define (positivos xs)
  (filter (lambda (x) (> x 0)) xs))

(displayln "Ejercicio 1:")
(displayln (positivos '(3 -2 7 0 -5 9)))

(define (cuadrados_pares xs)
  (map (lambda (x) (* x x))
       (filter even? xs)))

(displayln "Ejercicio 2:")
(displayln (cuadrados_pares '(1 2 3 4 5 6 7 8)))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;;

(displayln "Ejercicio 3:")
(displayln (factorial 5))

(define (cubos xs)
  (map (lambda (x) (* x x x)) xs))

(displayln "Ejercicio 4:")
(displayln (cubos '(2 3 4)))

(define (suma-impares xs)
  (foldl + 0 (filter odd? xs)))

(displayln "Ejercicio 5")
(displayln (suma-impares '(1 2 3 4 5 6 7)))

(define (contiene-negativos? xs)
  (ormap (lambda (x) (< x 0)) xs))

(displayln "Ejercicio 6")
(displayln (contiene-negativos? '(5 9 -3 2)))

(define (suma-acumulada xs)
  (reverse
   (foldl
    (lambda (x acc)
      (cons (+ x (if (null? acc) 0 (car acc))) acc))
    '()
    xs)))

(displayln "Ejercicio 7")
(displayln (suma-acumulada '(1 2 3 4)))

(define (concatenar-cadenas xs)
  (foldl string-append "" xs))

(displayln "Ejercicio 8")
(displayln (concatenar-cadenas '("Hola" " " "Mundo")))

(define (dobles-mayores-5 xs)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) xs)))

(displayln "Ejercicio 9")
(displayln (dobles-mayores-5 '(3 6 8 2 10)))

(define (invertir xs)
  (foldl (lambda (x acc) (cons x acc)) '() xs))

(displayln "Ejercicio 10")
(displayln (invertir '(1 2 3 4)))

(define (aplicar-funcion f xs)
  (map f xs))

(define (cuadrado x) (* x x))

(displayln "Ejercicio 11")
(displayln (aplicar-funcion cuadrado '(1 2 3 4)))

(define (promedio-mayores-5 xs)
  (let* ((mayores (filter (lambda (x) (> x 5)) xs))
         (suma (foldl + 0 mayores))
         (cantidad (length mayores)))
    (/ suma cantidad)))

(displayln "Ejercicio 12")
(displayln (promedio-mayores-5 '(3 8 10 4 9 2 7)))