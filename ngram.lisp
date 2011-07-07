(in-package :org.kjerkreit.ngram)

(defun gen-n-grams (strings &optional (n 2))
  "Generates a list containing n-grams (as lists) based on a list of strings.
N defaults to 2."

  (defun pad (string n)
    "Pads the strings before processing."

    (concatenate 'string
		 (make-string (1- n) :initial-element #\$)
		 string
		 (make-string (1- n) :initial-element #\$)))
  
  (defun iter (strings n-grams)
    "Iterates over the list of strings to produce the grams."

    (if strings
	(progn
	  (let ((string (pad (car strings) n))
		(grams nil))

	    (dotimes (i (- (length string) (1- n)))
	      (setf grams (append grams (list (subseq string i (+ i n))))))

	    (iter (cdr strings) (append n-grams (list grams)))))
	n-grams))

  (iter strings nil))

(defun compare-strings (string1 string2 &optional (warp 1.0) (n 2))
  "Compare two strings and return a score between 0 and 1."

  (compare-n-grams (car (gen-n-grams string1 n)) (car (gen-n-grams string2 n) warp n)))

(defun compare-n-grams (g1 g2 &optional (warp 1.0) (n 2))
  "Compare two sets of n-grams and return a score between 0 and 1."

  (defun compare-members (list1 list2)
    "How many grams are shared, and how many are there in total?"
    
	   (let ((shared 0)
		 (total 0))
	     
	     (dolist (e list1)
	       (when (member e list2 :test #'equal) 
		 (setf same (1+ same)))
	       (setf all (1+ all)))
	     
	     (dolist (e list2)
	       (when (not (member e list1 :test #'equal))
		 (setf all (1+ all))))
	     
	     (cons same all)))

  (let* ((gram-stats (compare-members g1 g2))
	 (shared (car gram-stats))
	 (total (cdr gram-stats)))

    (if (< (abs (- warp 1.0)) 1e-9)
	(float (/ shared total))
	(float (/ (- (* total warp)
		     (* (- total shared) warp))
		     (* total warp))))))
		
   
