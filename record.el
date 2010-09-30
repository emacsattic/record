;; record.el --- Named structure types with named fields for emacs-list
;;
;; Author: Guillaume Marceau
;;
;; Copyright (C) 2002, Guillaume Marceau
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:

(require 'cl)

(defmacro record-def (type-symbol fields)
  "Defines the record type TYPE-SYMBOL with fields FIELDS. Afterward, you can
   construct datum of this type with (make TYPE-SYMBOL DATUM ...)) and take them
   appart with (get-f FIELD RECORD). The nth field in the definition matches with the nth 
   item when calling the field accessor function `get-f'. No symbol should be quoted."
  (mapcar '(lambda (i) (if (not (symbolp i)) (error "Not a symbol : %s" i))) fields)
  `(put ',type-symbol 'field-definition ',fields))

(defmacro record-undef (type-symbol)
  "Undo the effect of `field-def'"
  `(put ',type-symbol 'field-definition ,'nil))

(defmacro record-get-def (type-symbol)
  `(get ',type-symbol 'field-definition))

(defun let-record-process-body (src-symbol dest-symbol forms)
  (cond ((not forms) nil)
        ((eq forms src-symbol) dest-symbol)
        ((and (listp forms) (eq (car forms) 'let-record)) (let-record-process (nth 1 forms) (nth 2 forms) (cdddr forms)))
        ((listp forms) (cons (let-record-process-body src-symbol dest-symbol (car forms))
                             (let-record-process-body src-symbol dest-symbol (cdr forms))))
        (t forms)))
  

(defun let-record-process (symbol fielddef forms)
  (let ((type-symbol (gensym "field-def")))
    `(progn (record-def ,type-symbol ,fielddef)
            (progn ,@(let-record-process-body symbol type-symbol forms)))))

(defmacro let-record (symbol fielddef &rest forms)
  "Create a field type bound to SYMBOL which exists only 
for the duration of the evaluation of FORMS"
  (let-record-process symbol fielddef forms))

(defun field-of (field-symbol datum)
  (if (not datum) (error "Taking field of nil"))
  (if (or (not (listp datum))
          (not (symbolp (car datum)))) (error "Not a symbol-prefixed list : %s" datum))
  
  (let* ((type-symbol (car datum))
         (fields (or (get type-symbol 'field-definition)
                     (error "Type %s undefined" type-symbol)))
         (index (position field-symbol fields)))
    (if (not index) (error "No field `%s' in datum-type `%s'" field-symbol (car datum))
      (nthcdr (1+ index) datum))))
      
(defmacro get-f (field-symbol datum)
  "Access field FIELD-SYMBOL of DATUM, which was created by `make'"
  `(car (field-of ',field-symbol ,datum)))

(defmacro set-f (field-symbol datum value)
  "Set field FIELD-SYMBOL to VALUE in place"
  `(setcar (field-of ',field-symbol ,datum) ,value))

(defmacro with-f (field-symbol datum value)
  "Return a new record with the same field values as DATUM
expect for FIELD-SYMBOL, which will be VALUE"
  (let ((cell-symb (gensym "with-f-cell"))
        (datum-symb (gensym "with-f-datum"))
        (value-symb (gensym "with-f-value")))
    `(let* ((,datum-symb ,datum)
            (,value-symb ,value)
            (,cell-symb (field-of ',field-symbol ,datum-symb)))
       (maplist '(lambda (i) (if (eq i ,cell-symb) ,value-symb (car i))) ,datum-symb))))

(defmacro make (type-symbol &rest items)
  "Make a new datum of type TYPE-SYMBOL containing matching field ITEMS"
  (let ((fielddef (get type-symbol 'field-definition)))
    (cond ((not fielddef) (error "Field of type-symbol `%s' are not defined. Use `record-def' first" type-symbol))
          ((/= (length fielddef) (length items)) (error "The field type `%s' does not have %d fields. Try %d" type-symbol (length items) (length fielddef)))
          (t `(list ',type-symbol ,@items)))))


(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\(let-record\\)\\>" . font-lock-keyword-face)))
(put 'let-record 'lisp-indent-function 2)

(provide 'record)

