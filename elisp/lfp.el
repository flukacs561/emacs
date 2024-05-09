;; -*- lexical-binding: t; -*-
;; title:      lfp
;; date:       2023-01-25-13-22-13
;; tags:       elisp,prog
;; identifier: 20230125T132213
;; -----------------------

;;;*Constants

(defconst lfp--english-alphabet (list "a" "b" "c" "d" "e" "f" "g" "h" "i"
				      "j" "k" "l" "m" "n" "o" "p" "q" "r"
				      "s" "t" "u" "v" "w" "x" "y" "z")
  "The letters of the English alphabet.")

(defconst lfp--hungarian-alphabet (list "a" "á" "b" "c" "d" "e" "é" "f" "g" "h"
					"i" "í" "j" "k" "l" "m" "n" "o" "ó" "ö"
					"ő" "p" "q" "r" "s" "t" "u" "ú" "ü" "ű"
					"v" "w" "x" "y" "z")
  "The single-character letters of the Hungarian alphabet.")

(defconst lfp--digits (list "0" "1" "2" "3" "4"
			    "5" "6" "7" "8" "9")
  "The 10 digits.")

(defconst lfp--remove-accent-alist '(("á" . "a")
				     ("é" . "e")
				     ("í" . "i")
				     ("ö" . "o")
				     ("ő" . "o")
				     ("ó" . "o")
				     ("ü" . "u")
				     ("ű" . "u")
				     ("ú" . "u"))
  "The alist associating each accented letter of the Hungarian
alphabet to its non-accented counterpart.")

(defconst lfp--en-alphanumeric (append lfp--english-alphabet lfp--digits)
  "The letters of the English alphabet and the 10 digits.")

(defconst lfp--hu-alphanumeric (append lfp--hungarian-alphabet lfp--digits)
  "The letters of the Hungarian alphabet and the 10 digits.")

;;;*The parsing-object data structure
;;;
;;; A parser object is a 2-element list. The first element is the part that
;;; has already been parsed. The second element is the part that has not yet
;;; been parsed. Parsing moves the interpreted part of the unparsed string
;;; to the parsed part. A failed parse is reresented by a =nil= in the
;;; unparsed field. This is different from an empty parse, which is the empty
;;; string.
;;;
;;; In this file, there are two variants of this structure: one where the first
;;; part is a string, and another where it is a list of strings. (There is kind
;;; of a third one aswell, where the first part is =nil=. This represents a
;;; failed parse.) The second kind is supposed to be 'one level up' compared to
;;; the first one. This means that we want to parse the string components into
;;; a list: that is, we first parse a component, and then put it in a list.
;;; What we end up with is the list of correctly parsed coponent.
;;;
;;; This data structure is completely internal to the library, not exposed at all.

(defun lfp--string->parsing-object (string)
  "Creates an empty parsing object out of STRING."
  (list "" string))

(defun lfp--make-parsing-object (parsed unparsed)
  "Creates a parsing object with PARSED as the first part and
UNPARSED as the second."
  (list parsed unparsed))

(defun lfp-parsed (parsing-object) (car parsing-object))
(defun lfp-unparsed (parsing-object) (cadr parsing-object))

(defun lfp--parsing-donep (parsing-object)
  "Checks whether PARSING-OBJECT has empty second part."
  (string-empty-p (lfp-unparsed parsing-object)))

(defun lfp--parsing-failedp (parsing-object)
  "Checks whether PARSING-OBJECT is the result of a failed
parse."
  (null (lfp-parsed parsing-object)))

(defun lfp--parsing-emptyp (parsing-object)
  "Checks whether the recently parsed part is empty."
  (string-empty-p (lfp-parsed parsing-object)))

(defun lfp--first-unparsed-char (parsing-object)
  "Returns the first character of the unparsed part of
PARSING-OBJECT."
  (substring (lfp-unparsed parsing-object) 0 1))

(defun lfp--move-parsed-char (parsing-object)
  "Return a new parsing-object that has the first character of the
unparsed part of PARSING-OBJECT appended to its parsed part."
  (lfp--make-parsing-object (concat (lfp-parsed parsing-object)
				    (lfp--first-unparsed-char parsing-object))
			    (seq-drop (lfp-unparsed parsing-object) 1)))

;;;*Primitive parsers
;;;
;;; A primitive parser is a function that takes a string as its argument,
;;; and checks whether the the first character of the string matches
;;; certain criteria assumed by the parser. I.e., they parse the first
;;; character of the string. The parsing succeeds if the first character
;;; of the strings indeed matches the parser's criterion. It then creates
;;; a parsing-object with the parsed character as its parsed part, and the
;;; rest of the string as its unparsed part.
;;;
;;; The following functions -- if we take as our definition of a primitive
;;; parser the one given above -- are not primitive parsers. The are parser
;;; generators, since they take arguments related to their criteria, and
;;; return primitive parsers.

(defun lfp-parse-values (values)
  "Takes a list of values and returns a function that takes a string
and parses its first character. If the first character of the string
is in VALUES, then it returns the result of the successful parse,
otherwise the unsuccessful parsing-object."
  (lambda (string)
    (let ((parsing-object (lfp--string->parsing-object string)))
      (cond ((lfp--parsing-donep parsing-object)
	     (lfp--make-parsing-object "" ""))
	    ((member (lfp--first-unparsed-char parsing-object) values)
	     (lfp--move-parsed-char parsing-object))
	    (t (lfp--make-parsing-object nil
					 (lfp-unparsed parsing-object)))))))

(defun lfp-parse-value (value)
  "Takes a value and returns a function that takes a string and
parses its first character. If the first character if the string
is VALUE, then it returns the result of the successful parse,
otherwise the unsuccessful parsing object."
  (lambda (string)
    (funcall (lfp-parse-values (list value)) string)))

(defun lfp-parse-xs-to-ys (xy-alist)
  "Takes an alist and returns a function that takes a string and
parses its first character. If the first character of the string
is in XY-ALIST, then appends its associated character to the
parsed part of the output and removes it from the unparsed."
  (lambda (string)
    (let ((parsing-object (lfp--string->parsing-object string)))
      (cond ((lfp--parsing-donep parsing-object)
	     (lfp--make-parsing-object "" ""))
	    ((assoc (lfp--first-unparsed-char parsing-object)
		    xy-alist)
	     (lfp--make-parsing-object
	      (cdr (assoc (lfp--first-unparsed-char parsing-object)
			  xy-alist))
	      (seq-drop (lfp-unparsed parsing-object) 1)))
	    (t (lfp--make-parsing-object
		nil
		(lfp-unparsed parsing-object)))))))

(defun lfp-parse-x-to-y (x y)
  "Takes a pair of values and returns a functions that takes a
string and parses its first character. If the first character is
X, then appends Y to the parsed part of the output and removes it
from the unparsed part."
  (lambda (string)
    (funcall (lfp-parse-xs-to-ys (list (cons x y))) string)))

(defun lfp-ignore-char (char)
  "Takes a character and returns a function that takes a string
and parses its first character. If the first character is CHAR,
then remove it from the unparsed part, but do not add to the
parsed part."
  (lambda (string)
    (funcall (lfp-parse-x-to-y char "") string)))

(defun lfp-parse-xs-to-fxs (values f)
  "Takes a list of values and a function that acts on the values
and returns a function that takes a string and parses its first
character. If the first character is among VALUES, then the
F is applied to it and the result is appended to the parsed part
of the output and the original character is removed from the
unparsed part."
  (lambda (string)
    (let ((parsing-object (lfp--string->parsing-object string)))
      (cond ((lfp--parsing-donep parsing-object)
	     (lfp--make-parsing-object "" ""))
	    ((member (lfp--first-unparsed-char parsing-object) values)
	     (lfp--make-parsing-object (funcall f (lfp--first-unparsed-char
						   parsing-object))
				       (seq-drop (lfp-unparsed parsing-object) 1)))
	    (t (lfp--make-parsing-object nil (lfp-unparsed parsing-object)))))))

(defun lfp-parse-x-to-fx (value f)
  "Takes a value and a function that acts on the value and returns a
function that takes a string and parses the first character of the
string. If the first character if VALUE, then then F is applied to
it and the result is appended to the parsed part of the output and
the original character is removed from the unparsed part."
  (lambda (string)
    (funcall (lfp-parse-xs-to-fxs (list value) f) string)))

;;;*Parser Combinators
;;;
;;; The main idea behind functional parsing is that one can combine
;;; primitive parsers into more complex parsers. A parser combinator
;;; is a function that facilitates just that: it takes as arguments
;;; one or more parsers and optionally other parameters, and returns
;;; a parser.
;;;
;;; Throughout this documentation, a parser is a function that takes
;;; a string and according to rules specific to the parser, consumes
;;; part of its arguments, and creates a parsing-object out of it.
;;;
;;; Below are some elementary ways in which one can combine parsers.

(defun lfp-combine (combine parsed parsing-object)
  "This is the function that does the 'monadic' step. It takes a string
PARSED and a parsing object PARSING-OBJECT and combines them into a single
parsing object by adding PARSED to the parsed part of PARSING-OBJECT using
the supplied COMBINE function.

Some further explanation on the monadic part: essentially the endofunctor
takes a type and makes a parsing-object type out of it. The unit of the monad
if essentially 'lfp--string->parsing-object', (in fact, this is the
string-component of the natural transformation) and the multiplication is
implicit. If we considered PARSED and PARSING-OBJECT as the constituents of
a parsing-object over parsing-objects, then 'lfp-combine' would be the
multiplication."
  (lfp--make-parsing-object (funcall combine
				     parsed
				     (lfp-parsed parsing-object))
			    (lfp-unparsed parsing-object)))

(defun lfp-combine-strict (combine parsed parsing-object)
  "See 'lfp-combine' but it returns a failed parsing object if PARSING-OBJECT
is a failed parsing object."
    (if (lfp--parsing-failedp parsing-object)
	(lfp--make-parsing-object nil (concat parsed
					      (lfp-unparsed parsing-object)))
      (lfp--make-parsing-object (funcall combine
					 parsed
					 (lfp-parsed parsing-object))
				(lfp-unparsed parsing-object))))

(defun lfp-repeat (parser)
  "Takes a parser and returns a function that takes a string and
applies the parser to it until it fails. Then returns a
parsing-object that has in its parsed part all the successfully
parsed characters, and in its unparsed part the rest of the string
together with the character on which the parsing failed."
  (lambda (string)
    (let ((parse (funcall parser string)))
      (cond ((lfp--parsing-donep parse) parse)
	    ((lfp--parsing-failedp parse) parse)
	    (t (lfp-combine #'concat
			    (lfp-parsed parse)
			    (funcall (lfp-repeat parser)
				     (lfp-unparsed parse))))))))

(defun lfp-or (parser1 parser2)
  "Takes two parsers and returns a function that takes a string and
tries to apply PARSER1 to it. If it succeeds then returns the resulting
parsing-object. If it fails, the tries to apply PARSER2 similarly."
  (lambda (string)
    (let ((parse (funcall parser1 string)))
      (if (lfp--parsing-failedp parse)
	  (funcall parser2 string)
	parse))))

(defun lfp-seq (&rest parsers)
  "Takes an arbitrary number of parsers, and returns a functions that
takes a string and applies the parsers to the string in the order
they appear in the original call. If any of the parsers fail, the
whole parsing fails.

This is because we want to parse a sequence, and if the string does not
contain the sequence, then it must be considered malformed with regards
to the parser."
    (lambda (string)
      (if (null parsers)
	  (lfp--string->parsing-object string)
	(let* ((first-parser (car parsers))
	       (rest-of-parsers (cdr parsers))
	       (parse (funcall first-parser string)))
	  (if (lfp--parsing-failedp parse)
	      parse
	    (lfp-combine-strict #'concat
				(lfp-parsed parse)
				(funcall (apply #'lfp-seq rest-of-parsers)
					 (lfp-unparsed parse))))))))

(defun lfp-times (n parser)
  "Takes an integer and a parser and returns a function that takes a string
and applies PARSER N times to STRING. If at any point the parsing fails,
the whole parsing fails."
  (lambda (string)
    (if (= 0 n)
	(lfp--string->parsing-object string)
      (let ((parse (funcall parser string)))
	(if (lfp--parsing-failedp parse)
	    parse
	  (lfp-combine-strict #'concat
			      (lfp-parsed parse)
			      (funcall (lfp-times (- n 1) parser)
				       (lfp-unparsed parse))))))))

(defun lfp-until (parser until-parser)
  "Takes two parsers and returns a function that takes a string and
applies applies the first parser to the string until it can
succesfully apply the second parser. Then it returns the result of
the succesful parses."
  (lambda (string)
    (let ((test-parse (funcall until-parser string)))
      (if (lfp--parsing-failedp test-parse)
	  (let ((parse (funcall parser string)))
	    (if (lfp--parsing-failedp parse)
		parse
	      (lfp-combine #'concat
			   (lfp-parsed parse)
			   (funcall (lfp-until parser until-parser)
				    (lfp-unparsed parse)))))
	test-parse))))

(defun lfp-until-excl (parser until-parser)
  "Takes two parsers and returns a function that takes a string and
applies  the first parser to the string until it can succesfully
apply the second parser. Then it returns the parts successfully parsed
by the first parser."
  (lambda (string)
    (let ((test-parse (funcall until-parser string)))
      (if (lfp--parsing-failedp test-parse)
	  (let ((parse (funcall parser string)))
	    (if (lfp--parsing-failedp parse)
		parse
	      (lfp-combine #'concat
			   (lfp-parsed parse)
			   (funcall (lfp-until-excl parser until-parser)
				    (lfp-unparsed parse)))))
	(lfp--string->parsing-object (lfp-unparsed test-parse))))))

(defun lfp-repeat-list (parser)
  "Takes a parser and returns a function that takes a string and
applies it the parser to the string. The parsed part of the
resulting parsing-object is then added to a list. This is repeated
until the parsing fails. Then a parsing-object is returned thats
parsed part is the list of successfully parsed strings, and thats
unparsed part is the remaining part of the string."
    (lambda (string)
      (let ((parse (funcall parser string)))
	(cond ((lfp--parsing-donep parse)
	       (lfp--make-parsing-object (list (lfp-parsed parse))
					 (lfp-unparsed parse)))
	      ((lfp--parsing-failedp parse) parse)
	      (t (lfp-combine #'cons
			      (lfp-parsed parse)
			      (funcall (lfp-repeat-list parser)
				       (lfp-unparsed parse))))))))

(defun lfp-seq-list (&rest parsers)
  "Takes a list of parsers and returns a function that takes a string
and consecutively applies the parsers to the string while accumulating
the parsed parts of the successful parses into a list. Then returns a
parsing-object with parsed part the list and unparsed part the remaining
part of the string."
    (lambda (string)
      (if (null parsers)
	  (lfp--make-parsing-object nil string)
	(let* ((first-parser (car parsers))
	       (rest-of-parsers (cdr parsers))
	       (parse (funcall first-parser string)))
	  (cond ((lfp--parsing-failedp parse) parse)
		((lfp--parsing-emptyp parse)
		 (funcall (apply #'lfp-seq-list rest-of-parsers)
			  (lfp-unparsed parse)))
		(t (lfp-combine #'cons
				(lfp-parsed parse)
				(funcall (apply #'lfp-seq-list rest-of-parsers)
					 (lfp-unparsed parse)))))))))

(defun lfp-times-list (n parser)
  "Takes an integer and a parser and returns a function that takes
a string and applies PARSER N times to the string while accumuating
the parsed parts of the successful parses into a list. If all N
parsings succeed, then it returns a parsing-object with parsed part
the list and unparsed part the remaining part of the string. If any
of the N parses fail, the whole parsing fails."
  (lambda (string)
    (if (= 0 n)
	(lfp--make-parsing-object nil string)
      (let ((parse (funcall parser string)))
	(cons ((lfp--parsing-failedp parse) parse)
	      ((lfp--parsing-emptyp parse)
	       (funcall (lfp-times-list (- n 1) parser)
			(lfp-unparsed parse)))
	      (t (lfp-combine #'cons
			      (lfp-parsed parse)
			      (funcall (lfp-times-list (- n 1) parser)
				       (lfp-unparsed parse)))))))))
						       
		 
;;;*Denote parsers

(defun lfp-parse-structured-filename (filename)
  "Parses filenames with structure 'title_author.extension' into
a 3-element list of the author, the title and the extension."
  (flatten-list
   (funcall (lfp-seq-list (lfp-repeat (lfp-parse-values (cons "-" lfp--en-alphanumeric)))
			  (lfp-ignore-char "_")
			  (lfp-repeat (lfp-parse-values (cons "-" lfp--en-alphanumeric))))
	    filename)))

;; (lfp-parse-structured-filename "title-word_author-name.extension")

(defun lfp-separator-string->list (separator)
  "Takes a character and returns a function that takes a string
and splits the string at every SEPARATOR character into a list."
  (lambda (string)
    (let ((parser (lfp-seq (lfp-repeat (lfp-parse-values lfp--en-alphanumeric))
			   (lfp-ignore-char separator))))
      (lfp-parsed (funcall (lfp-repeat-list parser) string)))))

(defun lfp-comma-separated-string->list (string)
  "Splits STRING at every comma."
  (funcall (lfp-separator-string->list ",") string))

;; (lfp-comma-separated-string->list "tag1,tag2,tag3")

(defun lfp-transform-filename (string)
  "Makes STRING suitable to be a filename. That is
1. Replaces all spaces with dashes.
2. Removes all (Hungarian) accents from vowels.
3. Makes it all lowercase."
  (let ((parser (lfp-or (lfp-parse-xs-to-ys (cons (cons " " "-")
						  lfp--remove-accent-alist))
			(lfp-parse-values (cons "-" lfp--en-alphanumeric)))))
    (lfp-parsed (funcall (lfp-repeat parser) (downcase string)))))

;; (lfp-transform-filename "Ez egy cím")

(defun parse-denote-filename (filename)
  "If FILENAME is a valid denote-style filename, the function splits it into
a list of timestamp, title, tags and extension."
  (let* ((parse-timestamp (lfp-until-excl (lfp-parse-values (cons "T" lfp--digits))
					  (lfp-times 2 (lfp-parse-value "-"))))
	 (parse-title (lfp-until-excl (lfp-parse-values (cons "-" lfp--en-alphanumeric))
				      (lfp-times 2 (lfp-parse-value "_"))))
	 (parse-tags (lfp-repeat (lfp-parse-values (cons "_" lfp--en-alphanumeric))))
	 (parse (flatten-list
		 (funcall (lfp-seq-list parse-timestamp
					parse-title
					parse-tags
					(lfp-ignore-char "."))
			  filename))))
    (setf (caddr parse) (funcall (lfp-separator-string->list "_")
				 (caddr parse)))
    parse))

;; (parse-denote-filename "30230506T120101--some-title__tag1_tag2.org")

(defun parse-book-filename (filename)
  "Returns a 4 element list of the title, author, list of tags
and extension of FILENAME."
  (let* ((parse-title (lfp-until-excl (lfp-parse-values (cons "-" lfp--en-alphanumeric))
				      (lfp-times 2 (lfp-parse-value "-"))))
	 (parse-author (lfp-until-excl (lfp-parse-values (cons "-" lfp--en-alphanumeric))
				       (lfp-times 2 (lfp-parse-value "_"))))
	 (parse-tags (lfp-repeat (lfp-parse-values (cons "_" lfp--en-alphanumeric))))
	 (parse (flatten-list (funcall (lfp-seq-list parse-title
						     parse-author
						     parse-tags
						     (lfp-ignore-char "."))
				       filename))))
    (setf (caddr parse) (funcall (lfp-separator-string->list "_")
				 (caddr parse)))
    parse))

;; (parse-book-filename "kerodon-category--jacob-lurie__book_cat_maths.pdf")

(defun lfp-kebab-case->camel-case (string)
  "Takes a kebab-case string and turns it into camelCase."
  (let* ((camel (lfp-seq (lfp-ignore-char "-")
			 (lfp-parse-xs-to-fxs lfp--english-alphabet #'upcase)))
	 (parser (lfp-or camel
			 (lfp-parse-values lfp--en-alphanumeric))))
    (lfp-parsed (funcall (lfp-repeat parser) string))))

;; (lfp-kebab-case->camel-case "valami-es-meg-valami")

(provide 'lfp)
