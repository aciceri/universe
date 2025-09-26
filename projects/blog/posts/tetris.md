---
title: Vintage tetris written in Guile Scheme
tags: [scheme, tetris, guile, ncurses]
date: 2017-08-12
language: en
---

This isn't really recent but it worths to be mentionated on this site, about one
month ago I started reading the [Wizard book](https://mitpress.mit.edu/sicp/) and I became obsessed with this
fantastic language. The first real project I tried was this Tetris
implementation using [Guile](https://www.gnu.org/software/guile/Guile) (yep, his sanctity Stallman should be proud of me),
the code ~~was~~ is orrible but it allowed me to discover how to make things working
pratically. The SICP is a great book but it doesn't offer any real world
example.

![Xkcd comic n. 224 from here](/images/xkcd-224.jpg)
*Xkcd comic n. 224 from [here](https://xkcd.com/224/)*

So, I realized the first working version in only one night but then I added a
lot of features, e.g.

- Coloured blocks and a nice interface
- I made a block lie on 2 characters so that it looks more squared
- Levels that make the game faster
- Points system inspired by the classical Nintendo game

Obviously I used the good old NCurses library, luckily Guile provides an
official wrapper that is well documented.

{youtube:7FWsP1iKPCY}

I know I shouldn't upload code so bad but I really enjoyed writing it and I'm
afraid of losing it in the future, so...


```scheme
(use-modules (srfi srfi-19))
(use-modules (ice-9 format))
(use-modules (ncurses curses))

(define shape-I
  #2((1 1 1 1)))

(define shape-J
  #2((2 2 2)
     (0 0 2)))

(define shape-L
  #2((3 3 3)
     (3 0 0)))

(define shape-O
  #2((4 4)
     (4 4)))

(define shape-S
  #2((0 5 5)
     (5 5 0)))

(define shape-Z
  #2((6 6 0)
     (0 6 6)))

(define shape-T
  #2((7 7 7)
     (0 7 0)))

(define (random-shape)
  (list-ref (list shape-I shape-J shape-L shape-S shape-Z shape-O shape-T) (random 7)))

(define field-width 10)
(define field-height 20)
(define real-field (make-array 0 field-height field-width))

(define (rotate shape)
  (let* ((height (car (array-dimensions shape)))
	 (width (cadr (array-dimensions shape)))
	 (rotated (make-array 0 width height)))
    (array-index-map! rotated (lambda (x y)
			  (array-ref shape (- height y 1) x)))
    rotated))

(define (iter-matrix matrix function) ;apply function to every possibile (x, y)
  (let ((n (car (array-dimensions matrix)))
	(m (cadr (array-dimensions matrix))))
    (map (lambda (y)
	   (map (lambda (x)
		  (function x y)) (iota m))) (iota n))))

(define (draw-field field)
  (iter-matrix field
	       (lambda (x y)
		 (let ((cell (array-ref field y x)))
		   (cond
		    ((= cell 1)
		     (attr-set! win-field (logior A_BOLD (color-pair 1)))
		     (addstr win-field "[]" #:x (+ (* x 2) 1) #:y (+ y 1)))
		    ((= cell 2)
		     (attr-set! win-field (logior A_BOLD (color-pair 2)))
		     (addstr win-field "[]" #:x (+ (* x 2) 1) #:y (+ y 1)))
		    ((= cell 3)
		     (attr-set! win-field (logior A_BOLD (color-pair 3)))
		     (addstr win-field "[]" #:x (+ (* x 2) 1) #:y (+ y 1)))
		    ((= cell 4)
		     (attr-set! win-field (logior A_BOLD (color-pair 4)))
		     (addstr win-field "[]" #:x (+ (* x 2) 1) #:y (+ y 1)))
		    ((= cell 5)
		     (attr-set! win-field (logior A_BOLD (color-pair 5)))
		     (addstr win-field "[]" #:x (+ (* x 2) 1) #:y (+ y 1)))
		    ((= cell 6)
		     (attr-set! win-field (logior A_BOLD (color-pair 6)))
		     (addstr win-field "[]" #:x (+ (* x 2) 1) #:y (+ y 1)))
		    ((= cell 7)
		     (attr-set! win-field (logior A_BOLD (color-pair 7)))
		     (addstr win-field "[]" #:x (+ (* x 2) 1) #:y (+ y 1)))
		    ((= cell 0)
		     (attr-set! win-field (logior A_NORMAL (color-pair 8)))
		     (addstr win-field ".." #:x (+ (* x 2) 1) #:y (+ y 1)))
		    )))))

(define (erase-from-field field shape x y)
  (iter-matrix shape
	       (lambda (i j)
		 (if (not (= (array-ref shape j i) 0))
		     (array-set! field 0 (+ y j) (+ x i))))))

(define (add-to-field field shape x y)
  (iter-matrix shape
	       (lambda (i j)
		 (if (not (= (array-ref shape j i) 0))
		     (array-set! field (array-ref shape j i) (+ y j) (+ x i))))))

(define (check-collision field shape x y old-shape old-x old-y) ;check collision, x, y are the new coordinates
  (let ((collision #f))
    (iter-matrix shape (lambda (i j)
			 (if (or (>= (+ x i) field-width)
				 (< (+ x i) 0)
				 (>= (+ y j) field-height))
			     (set! collision #t))))
    (if (not collision)
	(begin
	  (erase-from-field field old-shape old-x old-y)
	  (iter-matrix shape (lambda (i j)
			       (if (and (not (= (array-ref field (+ y j) (+ x i)) 0))
					(not (= (array-ref shape j i) 0)))
				   (set! collision #t))))

    ))
    (not collision)))


(define (new-field field) ;erase lines if necessary and returns new field
  (let ((new-field (make-array 0 field-height field-width))
	(actual-line (- field-height 1))
	(lines-removed 0)) ;lines removed
    (for-each (lambda (y)
		(let ((erase-line #t))
		  (for-each (lambda (x)
			      (if (= (array-ref field y x) 0)
				  (set! erase-line #f))
			      ) (iota field-width))
		  (if (not erase-line)

		      (begin
			(for-each (lambda (x)
				    (array-set! new-field (array-ref field y x) actual-line x)
				    ) (iota field-width))
			(set! actual-line (- actual-line 1)))
		      (begin
			(set! lines (+ lines 1)) ;global variable
			(set! lines-removed (+ lines-removed 1))
			(if (= (modulo lines 5) 0) ;level up every 5 lines
			     (set! level (+ level 1))))) ;global variable

		  )
		) (reverse (iota field-height)))
    (set! points (+ points (* (* lines-removed lines-removed 10) (+ level 1))))
    new-field))

(define (update-stats lines level shape)
  (clear stats-win)
  (attr-on! stats-win (logior A_NORMAL (color-pair 8)))
  (addstr stats-win "Next:" #:x 1 #:y 1)
  (addstr stats-win "Line:" #:x 1 #:y 6)
  (addstr stats-win (format #f "~3,'0d" lines) #:x 7 #:y 7)
  (addstr stats-win "Level:" #:x 1 #:y 8)
  (addstr stats-win (format #f "~2,'0d" level) #:x 8 #:y 9)
  (addstr stats-win "Score:" #:x 1 #:y 10)
  (addstr stats-win (format #f "~4,'0d" points) #:x 6 #:y 11)



  (iter-matrix shape (lambda (i j)
		       (let ((n (array-ref shape j i)))
			 (if (not (= n 0))
			     (begin
			       (attr-set! stats-win (logior A_BOLD (color-pair n)))
			       (addstr stats-win "[]" #:x (+ (* i 2) 2) #:y (+ j 3)))))))

  (refresh stats-win))


(define previous-millis 0)
(define (current-millis)
  (let ((t (current-time time-process)))
    (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))
  ))

(define (do-every interval f)
  (if (>= (- (current-millis) previous-millis) interval)
      (begin
	(set! previous-millis (current-millis))
	(f))))

(define (exit!) (clear win-field) (clear stats-win) (clear stdscr) (endwin) (display "Correctly terminated\n"))

(define (lose p l)
  (attr-on! stats-win (logior A_NORMAL (color-pair 8)))
  (addstr stats-win "YOU LOSE" #:x 1 #:y (- field-height 2))
  (addstr stats-win "Press q" #:x 1 #:y (- field-height 1))
  (refresh stats-win)

  (let ((key (getch stdscr)))
    (while (not (eq? key #\q))
      (begin
	(set! key (getch stdscr))
	)))
  (exit!))


;Curses initialization
(define stdscr (initscr))
(define win-field (newwin (+ field-height 2) (+ (* field-width 2) 2) 0 0))
(define stats-win (newwin field-height 10 1 (+ (* field-width 2) 2)))

(noecho!)
(cbreak!)
(curs-set 0)
(keypad! stdscr #t)
(timeout! stdscr 0)
(start-color!)
(init-pair! 1 COLOR_RED COLOR_RED)
(init-pair! 2 COLOR_BLUE COLOR_BLUE)
(init-pair! 3 COLOR_CYAN COLOR_CYAN)
(init-pair! 4 COLOR_YELLOW COLOR_YELLOW)
(init-pair! 5 COLOR_GREEN COLOR_GREEN)
(init-pair! 6 COLOR_WHITE COLOR_WHITE)
(init-pair! 7 COLOR_MAGENTA COLOR_MAGENTA)
(init-pair! 8 COLOR_WHITE COLOR_BLACK)


(set! *random-state* (seed->random-state (time-second (current-time time-utc))))


;Game initializazion
(define tet-x (- (quotient field-width 2) 1))
(define tet-y 0)
(define tet-shape (random-shape))
(define next-shape (random-shape))
(define go #t)
(define key)
(define lines 0)
(define level 0)
(define points 0)


(while go ;Game logic here


  (draw-field real-field)
  (refresh win-field)

  (update-stats lines level next-shape)


  (do-every (- 600 (* level 50)) (lambda ()
		  (erase-from-field real-field tet-shape tet-x tet-y)
		  (if (not (check-collision real-field tet-shape tet-x (+ tet-y 1) tet-shape tet-x tet-y))
		       (begin
			 (add-to-field real-field tet-shape tet-x tet-y)

			 (set! real-field (new-field real-field))

			 (set! tet-x (- (quotient field-width 2) 1))
			 (set! tet-y 0)
			 (set! tet-shape next-shape)
			 (set! next-shape (random-shape))

			 (if (not (check-collision real-field tet-shape tet-x (+ tet-y 1) tet-shape tet-x tet-y))
			     (begin
			       (add-to-field real-field tet-shape tet-x (+ tet-y 1))
			       (draw-field real-field)
			       (refresh win-field)
			       (set! go #f)
			       (lose level lines)))



			 )



		       (set! tet-y (+ tet-y 1))
		       )
		  (add-to-field real-field tet-shape tet-x tet-y)
		   ))

  (attr-on! win-field (logior A_NORMAL (color-pair 8)))
  (box win-field (acs-vline) (acs-hline))


  (set! key (getch stdscr))
  (cond
   ((eqv? key #\q)
    (exit!) (set! go #f))

   ((eqv? key #\r)
    (let ((shape-rotated (rotate tet-shape)))
      (if (check-collision real-field shape-rotated tet-x tet-y tet-shape tet-x tet-y)
	  (begin
	    (erase-from-field real-field tet-shape tet-x tet-y)
	    (set! tet-shape shape-rotated)
	    (add-to-field real-field tet-shape tet-x tet-y)))))

   ((eqv? key KEY_RIGHT)
    (if (check-collision real-field tet-shape (+ tet-x 1) tet-y tet-shape tet-x tet-y)
	(begin
	  (erase-from-field real-field tet-shape tet-x tet-y)
	  (set! tet-x (+ tet-x 1))
	  (add-to-field real-field tet-shape tet-x tet-y))))

   ((eqv? key KEY_LEFT)
    (if (check-collision real-field tet-shape (- tet-x 1) tet-y tet-shape tet-x tet-y)
        (begin
	  (erase-from-field real-field tet-shape tet-x tet-y)
	  (set! tet-x (- tet-x 1))
	  (add-to-field real-field tet-shape tet-x tet-y))))

   ((eqv? key KEY_DOWN)
    (if (check-collision real-field tet-shape tet-x (+ tet-y 1) tet-shape tet-x tet-y)
        (begin
	  (erase-from-field real-field tet-shape tet-x tet-y)
	  (set! tet-y (+ tet-y 1))
	  (add-to-field real-field tet-shape tet-x tet-y))))

   ((eqv? key #\p)
    (begin
      (attr-on! stats-win (logior A_NORMAL (color-pair 8)))
      (addstr stats-win "-PAUSE-" #:x 1 #:y (- field-height 1))
      (refresh stats-win)
      (while (not (eqv? (getch stdscr) #\p))
	'())))
   )


  (refresh win-field)
  )
```