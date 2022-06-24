(in-package #:cl-user)
(defpackage taylor-impossible-game-sim
  (:use :cl)
  (:export :impossible-game))
(in-package #:taylor-impossible-game-sim)

(defclass player ()
  ((%id :initform (incf *player-ids*)
        :accessor id)
   (%position :initform 1
              :accessor pos)
   (%turn :initform 0
          :accessor turn)
   (%skip-turn :initform 0
               :accessor skip-turn)
   (%die :initform 6
         :accessor die)
   (%temp-die :initform nil
              :accessor temp-die)
   (%knife :initform nil
           :accessor knife)
   (%1st-clue :initform nil
              :accessor 1st-clue)
   (%2nd-clue :initform nil
              :accessor 2nd-clue)
   (%3rd-clue :initform nil
              :accessor 3rd-clue)
   (%scale :initform 0
           :accessor scale)))

;; NOTE: this kinda works for tracking any time a player is moved but instead I use RECORD-POSITION
;; at the end of each turn to track their ending position
;; (defmethod (setf pos) :after (new-pos (p player))
;;   "Record the number of times we touch each square."
;;   (when *debug* (format t "REC POSITION~%"))
;;   (if (gethash (pos p) *square-hits*)
;;       (incf (gethash (pos p) *square-hits*))
;;       (setf (gethash (pos p) *square-hits*) 1)))

(defvar *debug* nil)
(defvar *players* nil)
(defvar *player-ids* 0)
(defparameter *square-hits* (make-hash-table))

(defun record-position (player)
  (if (gethash (pos player) *square-hits*)
      (incf (gethash (pos player) *square-hits*))
      (setf (gethash (pos player) *square-hits*) 1)))

(defun roll (player)
  "Roll the type of die PLAYER is using."
  (let ((n (or (temp-die player)
               (die player))))
    (let ((roll (1+ (random n))))
      (when *debug* (format t "P~d Rolled ~d/~d~%" (id player) roll n))
      roll)))

(defun swap-with-last-player (player)
  (unless (< (length *players*) 2)
    (setf (pos player) (loop for player in *players*
                             minimize (pos player)))))

(defun move (player)
  "Move PLAYER based on roll."
  (let ((stop-here (cond ((< (pos player) 7)  7)
                         ((< (pos player) 20) 20)
                         (t                   25))))
    (setf (pos player)
          (min stop-here (+ (pos player) (roll player))))))

(defun move-spaces (player n)
  (incf (pos player) n))

(defun play-uno ()
  "Assume a random player loses. Send them to the start."
  (setf (pos (nth (random (length *players*)) *players*)) 1))

(defun mystery-solved-p (player)
  (and (1st-clue player) (2nd-clue player) (3rd-clue player)))

(defun take-action (player)
  (when (case (pos player)
          ;; Do nothing
          ((1 16 21 24) nil)
          ;; Back to start and miss a turn
          (3 (progn (setf (pos player) 1) (incf (skip-turn player) 1) nil))
          ;; Back to start
          (4 (setf (pos player) 1))
          (12 (if (1st-clue player)
                  nil
                  (setf (pos player) 1)))
          ;; Go forward/back spaces
          (13 (move-spaces player (- (roll player))))
          ;; Skip n turns
          (2 (progn (incf (skip-turn player) 10) nil))
          (11 (progn (incf (skip-turn player)) nil))
          ;; Change die
          (5 (progn (setf (temp-die player) 4) nil))
          (9 (progn (setf (die player) 8) nil))
          (10 (progn (setf (die player) 4) nil))
          ;; boing!
          (6 (setf (pos player) 15))
          ;; Thorns
          (7 (if (knife player)
                 nil
                 (progn (incf (skip-turn player) 2) nil)))
          ;; Clues
          (8 (progn (setf (1st-clue player) t) nil))
          (14 (progn (setf (2nd-clue player) t) nil))
          (18 (progn (setf (3rd-clue player) t) nil))
          ;; Scale
          (15 (progn (incf (scale player)) nil))
          ;; Uno (randomly pick loser)
          (17 (progn (play-uno) nil))
          (19 (when (oddp (roll player)) (setf (pos player) 11)))
          (20 (when (mystery-solved-p player)
                (setf (knife player) t) nil))
          (22 (setf (pos player) (+ 1 (roll player) (roll player) (roll player))))
          (23 (progn (swap-with-last-player player) nil))
          (25 (unless (mystery-solved-p player)
                (setf (pos player) 1)
                (incf (skip-turn player)))))
    (take-action player)))

(defun take-turn (player)
  (incf (turn player))
  (when (> (skip-turn player) 0)
    (when *debug* (format t "P~d | Turn ~d | SKIPPED~%" (id player) (turn player)))
    (decf (skip-turn player))
    (return-from take-turn))
  (move player)
  (take-action player)
  (record-position player)
  ;; Send smaller players back to the start
  (loop for other-player in *players*
        when (and (= (pos other-player) (pos player))
                  (< (scale other-player) (scale player)))
          return (setf (pos other-player) 1))
  (when *debug* (format t "P~d | Turn ~d | Square ~d~%" (id player) (turn player) (pos player)))
  (setf (temp-die player) nil))

(defun impossible-game (num-players)
  (let* ((*player-ids* 0)
         (*players* (loop repeat num-players collect (make-instance 'player))))
    (do ((game-over-p nil)
         (winner nil))
        (game-over-p (when *debug* (format t "P~d wins!" (id winner))) (turn winner))
      (setf game-over-p (loop for player in *players*
                              do (take-turn player)
                              when (and (= (pos player) 25)
                                        (mystery-solved-p player)
                                        (setf winner player))
                                return t)))))

(defun average-turns (sample-size num-players)
  (/ (loop repeat sample-size summing (impossible-game num-players))
     sample-size))
