;; -*- Mode: Emacs-Lisp -*-
;; pick-a-string.el -- Pick a random string from a file.
;; Copyright(C) 2003 Daisuke Kakura "2009-07-02 Thu 14:40:53"

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;; HISTORY:
;;
;; 2009-07-02	v0.1b	 Daisuke Kakura: added delimiter :
;; 2004-09-20	v0.1a	 Daisuke Kakura: bug fixed.
;; 2003-10-07	v0.1	 Daisuke Kakura: created.

;; INSTALL:
;;
;;    (require 'current-string-to)
;;
;; I use this key binding.
;;
;;    (global-set-key "\C-t" 'current-string-to-copy)
;;
;; If you write and read Japanese, try this.
;;
;;    (setq current-string-to-delimiter-regexp current-string-to-delimiter-regexp-jp)

;; HOW TO USE:
;;
;;	  --->  Quick Copy and Search
;;
;;	At first, bind `current-string-to-copy' to some key such as \C-t.
;; Now you can copy currently pointed word or string by C-t. Then hit C-sC-s or
;; C-rC-r (`isearch-forward' or `isearch-backward') twice. You can easily search
;; the word or string that cursor is on.
;;
;; It updates isearch-ring automatically. If you don't like it, do this;
;;
;;		(setq current-string-to-update-search-ring nil)
;;
;; If you want it to automatically update regexp-search-ring too, do this;
;;
;;		(setq current-string-to-update-regexp-search-ring t)
;;
;;	  --->  Quick Mark a Word, String or Line
;;
;; Hit C-SpaceC-Space (set mark twice at the same point) to mark currently
;; pointed word or string. If you hit C-SpaceC-SpaceC-Space (3 times), a whole
;; line will be marked.

;; TODO:
;;
;; * Better Japanese character handling.
;;	  ... to recognize Kanji, Hiragana, Katakana, Hankana and etc.


(defvar current-string-to-delimiter-regexp-ascii
	"[\][\t\(\)\"\|\\ /`',;:<>{}¢£=]"
	"A regular expressions to search ascii delimiters.")

(defvar current-string-to-delimiter-regexp-jp
	"[\][\t\(\)\"\|\\ /`',;:<>{}¢£=Å@-ÅGÅaÅbÅeÅg-ÅzÅÉÅÑÅ·Å‚Ñü-Ñæ]"
	"A regular expressions to search ASCII and Japanese delimiters.")

(defvar current-string-to-delimiter-regexp
	current-string-to-delimiter-regexp-ascii
	"A regular expressions to use for actual searching.")

(defvar current-string-to-update-search-ring t
	"If t, the string copied by `current-string-to-copy' will be put on the top of isearch-ring.")

(defvar current-string-to-update-regexp-search-ring nil
	"If t, the string copied by `current-string-to-copy' will be put on the top of regexp-isearch-ring.")

(defvar current-string-to-mark-point-at-beginning nil
	"If t, cursor goes to at the beginning of the mark after `current-string-to-mark'.")

;(defvar current-string-to-mark-point-stay t
;	"If t, cursor stays where it is.")	... is todo.

(defconst current-string-to-mark nil "Is mark set now?")

(global-set-key [?\C- ] 'current-string-to-mark)


(defun current-string-to-copy ()
	"If `current-string-to-update-search-ring' is t,
copy the pointed string and put it on the top of isearch-ring.
If `current-string-to-update-regexp-search-ring' is t,
copy the pointed string and put it on the top of regexp-isearch-ring."
	(interactive)
	(save-excursion
		(copy-region-as-kill (current-string-to-go-backward-delimiter)
									(current-string-to-go-forward-delimiter))
		(message (current-string-to-get-last-kill)))
	(let ((last-kill-str (current-string-to-get-last-kill)))
		(progn
			(unless (eq (length last-kill-str) 0)
				(if current-string-to-update-search-ring
					(isearch-update-ring last-kill-str))
				(if current-string-to-update-regexp-search-ring
					(isearch-update-ring last-kill-str regexp-search-ring))))))

(defun current-string-to-mark ()
	"If you set `Mark' twice at the same point, most likely by C-SPC C-SPC,
then mark currently pointed string. If you set `Mark' three times, mark whole line."
	(interactive)
	(cond ((and mark-active (eq current-mark-pos (point)) (not current-string-to-mark))
				(push-mark (current-string-to-go-backward-delimiter) nil t)
				(current-string-to-go-forward-delimiter)
				(setq current-string-to-mark t)
				(if current-string-to-mark-point-at-beginning
					(exchange-point-and-mark))
				(setq current-mark-pos (point)))
			((and current-string-to-mark (eq current-mark-pos (point)))
				(push-mark (beginning-of-line) nil t)
				(end-of-line)
				(setq current-string-to-mark nil)
				(if current-string-to-mark-point-at-beginning
					(exchange-point-and-mark)))
			(t
				(push-mark (point) nil t)
				(setq current-mark-pos (point))
				(setq current-string-to-mark nil))))

(defun current-string-to-go-forward-delimiter ()
	(while (not (or (looking-at current-string-to-delimiter-regexp) (eolp)))
		(forward-char 1))
	(point))

(defun current-string-to-go-backward-delimiter ()
	(let ((point-before-move (point)))
		(progn
			(while (not (or (looking-at current-string-to-delimiter-regexp) (bolp)))
				(backward-char 1))
			(if (and (not (eq point-before-move (point))) (looking-at current-string-to-delimiter-regexp))
				(forward-char 1))))
	(point))

(defun current-string-to-get-last-kill ()
	"Returns a value of the last killed string."
	(let ((str (copy-sequence (current-kill 0 t))))
		(set-text-properties 0 (length str) nil str)
	 str))

(defun current-string-to-get-last-mark ()
	"Returns the last marked point."
	(string-match "[0-9]+" (prin1-to-string (car mark-ring)))
	(string-to-int (match-string 0 (prin1-to-string (car mark-ring)))))

(provide 'current-string-to)

;;; current-string-to.el ends here.
