(in-package #:cl-user)
(defpackage impossible-game-sim
  (:use :cl)
  (:export :impossible-game))
(in-package #:impossible-game-sim)

(defclass player ()
  ((%id :initform (incf *player-ids*)
        :accessor id)
   (%position :initform 1
              :accessor pos)
   (%turn :initform 0
          :accessor turn)
   (%skip-turn :initform 0
               :accessor skip-turn)))

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

(defun roll (n)
  (let ((roll (1+ (random n))))
    (when *debug* (format t "Rolled ~d~%" roll))
    roll))

(defun swap-with-last-player (player)
  (unless (< (length *players*) 2)
    (setf (pos player) (loop for player in *players*
                             minimize (pos player)))))
(defun move (player)
  "Move PLAYER based on roll."
  (if (= (pos player) 38)    ; when sitting on 38
      (when (= (roll 20) 17) ; win condition
        (incf (pos player)))
      (setf (pos player)     ; don't pass 38
            (min 38 (+ (pos player) (roll 6))))))

(defun move-spaces (player n)
  (incf (pos player) n))

(defun take-action (player)
  (when (case (pos player)
          ;; Do nothing
          ((1 4 8 9 11 13 15 16 19 20 22 23 24 25 27 30 31 35 36 38 39) nil)
          ;; Back to start
          ((2 3 5 18 28 37) (setf (pos player) 1))
          ;; Go forward/back spaces
          ((10 17) (move-spaces player -1))
          (6       (move-spaces player -2))
          (12      (move-spaces player -6))
          (29      (move-spaces player -4))
          (33      (move-spaces player -3))
          (34      (move-spaces player -8))
          (26      (move-spaces player 4))
          ;; Skip n turns
          (14 (progn (incf (skip-turn player)) nil))
          (21 (progn (incf (skip-turn player) 2) nil))
          (7 (when (= (turn player) 1) (setf (pos player) 1)))
          (32 (progn (swap-with-last-player player) nil)))
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
  (when *debug* (format t "P~d | Turn ~d | Square ~d~%" (id player) (turn player) (pos player))))

(defun impossible-game (num-players)
  (let* ((*player-ids* 0)
         (*players* (loop repeat num-players collect (make-instance 'player))))
    (do ((game-over-p nil)
         (winner nil))
        (game-over-p (when *debug* (format t "P~d wins!" (id winner))) (turn winner))
      (setf game-over-p (loop for player in *players*
                              do (take-turn player)
                              when (and (= (pos player) 39) (setf winner player)) return t)))))

(defun average-turns (sample-size num-players)
  (/ (loop repeat sample-size summing (impossible-game num-players))
     sample-size))
