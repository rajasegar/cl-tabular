(in-package :cl-user)
(defpackage cl-tabular.web
  (:use :cl
        :caveman2
        :cl-tabular.config
        :cl-tabular.view
        :cl-tabular.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :cl-tabular.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defvar *foods* nil)

(defvar *dishes* '("Pizza"
		   "Noodles"
		   "Fried Rice"
		   "Roti"
		   "Lasagna"
		   "Churros"
		   "Tea"
		   "Soup"
		   "Egg roll"
		   "Salad"
		   "Burger"
		   "Rice"
		   "Curry"
		   "Bread"))

(defvar *cuisines* '("Indian"
		     "Chinese"
		     "Thai"
		     "Continental"
		     "Mexican"
		     "Indonesian"
		     "Japanese"
		     "Spanish"
		     "Italian"
		     "Greek"))

(defun random-elt (mylist)
  "Get random element from a list"
  (elt mylist (random (length mylist))))

;; Clear the list
(setf *foods* nil)

;; Push 100 items into foods with random values
(dotimes (i 100)
  (push (list :name (random-elt *dishes*)
	      :cuisine (random-elt *cuisines*)
	      :rating (+ 1 (random 5))
	      :price (+ 1 (random 100))) *foods*))

(defun slice-list (start the-list)
  "Slice the list with 10 items from the start index"
  (let ((new-list nil))
    (dotimes (i 10)
      (push (elt the-list (+ i start)) new-list))
    (reverse new-list)))

(defun query-param (name parsed)
  "Parse query param values"
  (cdr (assoc name parsed :test #'string=)))

(defun generate-pages ()
  "Generate pagination"
  (let ((pages nil))
    (dotimes (i 10)
      (push (list :id (+ 1 i) :start (* 10 i)) pages))
    (reverse pages)))


(defun get-opposite-direction (direction)
  "Get the opposite direction"
  (if (string= direction "asc")
      "desc"
      "asc"))

(defun sort-list (direction sort-by)
  "Sort a list based on the direction and key"
  (cond ((string= sort-by "name") (sort-list-by-name direction))
	((string= sort-by "rating") (sort-list-by-rating direction))
	((string= sort-by "price") (sort-list-by-price direction))
	((string= sort-by "cuisine") (sort-list-by-cuisine direction))))

(defun sort-list-by-name (direction)
  "Sort a list by name"
  (let ((sort-fn (if (string= direction "asc") #'string< #'string>)))
    (sort (copy-list *foods*) sort-fn :key (lambda (plist) (getf plist :name)))))

(defun sort-list-by-rating (direction)
  "Sort a list by rating"
  (let ((sort-fn (if (string= direction "asc") #'< #'>)))
    (sort (copy-list *foods*) sort-fn :key (lambda (plist) (getf plist :rating)))))

(defun sort-list-by-price (direction)
  "Sort a list by price"
  (let ((sort-fn (if (string= direction "asc") #'< #'>)))
    (sort (copy-list *foods*) sort-fn :key (lambda (plist) (getf plist :price)))))

(defun sort-list-by-cuisine (direction)
  "Sort a list by price"
  (let ((sort-fn (if (string= direction "asc") #'string< #'string>)))
    (sort (copy-list *foods*) sort-fn :key (lambda (plist) (getf plist :cuisine)))))

(defroute "/" (&key _parsed)
  (format t "_parsed = ~a~%" _parsed)
  (let ((start (parse-integer (or (query-param "start" _parsed) "0")))
        (direction (or (query-param "direction" _parsed) "asc"))
        (sort-by (or (query-param "sort-by" _parsed) "name")))
    (render #P"index.html"
            (list
             :foods (slice-list start (sort-list direction sort-by))
             :total (length *foods*)
	     :pages (generate-pages)
	     :start start
	     :direction direction
	     :sort-by sort-by
	     :opposite-direction (get-opposite-direction direction)))))

(defun filter-foods (query)
  "Filter foods based on the query with name"
  (remove-if #'(lambda (food)
                 (let ((name (getf food :name)))
                   (if (search query name :test #'char-equal)
                       nil
                       t))) *foods*))

(defroute ("/search" :method :POST) (&key _parsed)
  (format t "_parsed = ~a~%" _parsed)
  (let* ((query (cdr (assoc "query" _parsed :test #'string=)))
        (filtered-foods (filter-foods query)))
    (render #P"_search.html"
            (list
             :foods filtered-foods
             :total (length filtered-foods)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
