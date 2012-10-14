;;; erblisp.el --- 
;; Time-stamp: <2007-11-23 11:30:08 deego>
;; Copyright (C) 2002 D. Goel, 2012 Joel McCracken
;; Emacs Lisp Archive entry
;; Filename: erblisp.el
;; Package: erblisp
;; Author: Joel McCracken <mccracken.joel@gmail.com>, D. Goel <deego@gnufans.org>
;; Version: 0.0DEV
;; URL:  
 

 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 


;; this file is adapted from the code from erbot. see:

;; http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot

;; eventually I hope to make it better, but for now 
;; other projects can use the same sandboxing functionality



(defvar sandbox-prefix "emacs-sandbox-")
(defvar sandbox-allowed-words
  '(nil
    t 
    ;; Also consider:
    &rest
    &optional
    )
  "You should add &rest and &optional to this list. 
We WON'T do this by default since this could lead to exploits if you
*happen* to have bound these keywords to weird stuff like 
\(setq &rest (shell-command \"rm -rf /\")) in your .emacs."
  )



;; main entry point is sandbox
;; sandbox takes an expression and makes sure it is okay to evaluate

;; functions wich start with 'sandbox--' should be considered private

(defun sandbox (expr)
  "sandboxes an expression so that it doesn't fail"
  (cond
   ;; first condition
   ((null expr) nil)
   ;; second condition
   ((listp expr)
    (when (sandbox--check-args expr)
      (let ((fir (first expr)))
        (cond
         ((listp fir)
          (cons (sandbox fir)
                (mapcar 'sandbox (cdr expr))))
         ((equal (format "%S" fir) "quote")
          ;; if quoted, it is fine...
          expr)
         (t (cons 
             (if (or (equal 0 (string-match sandbox-prefix (format "%S" fir)))
                     (member fir sandbox-allowed-words))
                 fir
               (intern (concat sandbox-prefix (format "%S" fir))))
             (mapcar 'sandbox (cdr expr))))))))

   ;; final condition.. --> when the expr is an atom..  It should be a
   ;; a constant..  or an allowed atom.. allowed == prefixed with fs-
   (t (cond
       ((and (symbolp expr) 
             (equal 0 (string-match sandbox-prefix (format "%s" expr))))
        expr)
       ((equal expr t) expr)
       ((member expr sandbox-allowed-words) expr)
       ((symbolp expr)
        ;;(boundp (intern (concat sandbox-prefix (format "%S" expr)))))
        (intern (concat sandbox-prefix (format "%s" expr))))
       ;; other symbol
       ;;((symbolp expr) (list 'quote expr))
       ;; a number or string now..
       ;; this actually happens when they feed byte-compiled code to
       ;; the bot, like:
       ;;, (funcall #[nil "\300\207" [1] 1])    
       ((not (or (symbolp expr) (numberp expr) (stringp expr)))
        (error "%s %s" "Should not reach here.  Quantum Tunnelling! "
               "What are you trying to feed me? Byte-compiled code? Vectors?"  ))
       (t expr)))
   ))


(defvar sandbox-max-list-length 100)

(defmacro sandbox--check-args (&rest args)
  "All we do in this macro we remove some bindings for things like
&rest, etc, things that do not have values but got passed to us --
this occurs when a user attempts to use &rest in his function
definitions -- see `sandbox-allowed-words'.  

All the arguments to this macro should have been in their evalled form
and hence constants already, so we do not bother protecting against
multiple evaluations here -- evaluating a constant causes no harm.
All we do in this macro we remove some bindings for things like &rest,
etc, things that are not defined, but passed on here in any case."
  `(sandbox--check-args-nascent 
    ,@(remove-if 
       #'(lambda (arg) (and
                   (symbolp arg) 
                   (not (boundp arg)))) 
       args)))




(defun sandbox--check-args-nascent (&rest args)
  (if (or 
       (not (numberp sandbox-max-list-length))
       (sandbox--safe-length-args-p args 0 sandbox-max-list-length))
      t
    (error "encountered overlong expression, ignoring") nil))


(defun sandbox--safe-length-args-p (list so-far len)
  (let ((cur list)
        stack)
    (while (and cur
                (<= so-far len))
      (if (consp (car cur))
          (progn (setq cur (car cur))
                 (when (consp (cdr cur))
                   (push (cdr cur) stack)))
        (setq cur (cdr cur)))
      (unless cur
        (setq cur (pop stack)))
      (setq so-far (1+ so-far)))
    (if (<= so-far len)
        t
      nil)))

(provide 'sandbox)
