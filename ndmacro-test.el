;;; ndmacro-test.el --- test for ndmacro.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  ril

;; Author: ril <fenril.nh@gmail.com>
;; Keywords: convenience

;;; Commentary:

;;

;;; Code:
(require 'ert)
(require 'ndmacro)

(ert-deftest ndmacro-list-shift-1 ()
  (should (equal '((1 2) (3 4 5 6))
                 (ndmacro-list-shift
                  '(1 2 3) '(4 5 6)))))

(ert-deftest ndmacro-seq-prefix-matched-1 ()
  (should (equal '(2 1 0 "A")
                 (ndmacro-seq-prefix-matched
                  '(2 1 0 "A" 5 4 3) '(2 1 0 "A" "H" "O")))))

(ert-deftest multiple-value-list-1 ()   ; fail
    (should (equal '((("a" "b" "a" "b" "c" "c") ("a" "b" "a" "b" "c" "c")) 0)
                   (multiple-value-list
                    (ndmacro-search-loop
                     '("a" "b" "a" "b" "c" "c"
                       "a" "b" "a" "b" "c" "c" "d" "e" "f" "g"))))))

(ert-deftest ndmacro-search-loop-1 ()   ; fail
    (should (equal '((("a" "b") ("a" "b")) 0)
                   (ndmacro-search-loop
                    '("a" "b" "a" "b" "c" "-"
                      "a" "b" "a" "b" "c" "c" "d" "e" "f" "g")))))

(ert-deftest ndmacro-search-loop-2 ()
    (should (equal '((("a" "b") ("a" "b")) 0)
                   (ndmacro-search-loop
                    '("a" "b"
                      "a" "b")))))

(ert-deftest ndmacro-search-loop-3 ()
    (should (equal '((("a" "b") ("a" "b") ("a" "b")) 0)
                   (ndmacro-search-loop
                    '("a" "b"
                      "a" "b" "a" "b")))))

(ert-deftest ndmacro-search-loop-4 ()
    (should (equal '((("a" "b" "a" "b") ("a" "b" "a" "b")) 0)
                   (ndmacro-search-loop
                     '("a" "b" "a" "b"
                       "a" "b" "a" "b")))))

(ert-deftest ndmacro-search-loop-5 ()
  (should (equal '(((49 48 49 44) (49 48 50 44) (49 48 51 44)) 0)
                 (ndmacro-search-loop
                  '(49 48 49 44
                       49 48 50 44
                       49 48 51 44)))))

(ert-deftest ndmacro-search-loop-6 ()
  (should (equal '(((5 4 3 2 1 0 "A") (5 4 3 2 1 0 "A")) 4)
                 (ndmacro-search-loop
                  '(2 1 0 "A" 5 4 3 2 1 0 "A" "H" "O")))))

(ert-deftest ndmacro-predict-repeat-1 () ; fail
  (should (equal ' ((("A" 0 1 2 3 4 5) ("A" 0 1 2 3 4 5)) 4)
                   (ndmacro-predict-repeat
                    '(2 1 0 "A" 5 4 3 2 1 0 "A" "H" "O")))))

(ert-deftest ndmacro-split-seq-if-1 ()
  (should (equal '((49 51) (49 52))
                 (ndmacro-split-seq-if
                  'identity '(49 51 nil 49 52 nil)))))

(ert-deftest ndmacro-split-seq-if-2 ()
  (should (equal '((49 51) (49 52))
                 (ndmacro-split-seq-if
                  'identity '(nil 49 51 nil 49 52 nil)))))

(ert-deftest ndmacro-split-seq-if-3 ()
  (should (equal '((49 51) (49 52))
                 (ndmacro-split-seq-if
                  'identity '(nil 49 51 nil 49 52)))))

(ert-deftest ndmacro-position-subseq-1 ()
  (should (= 1
             (ndmacro-position-subseq
              '(nil 49 51 nil 49 52) '(49 51)))))

(ert-deftest ndmacro-position-subseq-2 ()
  (should (= 4
             (ndmacro-position-subseq
              '(nil 49 51 nil 49 52) '(49 52)))))

(ert-deftest ndmacro-get-numbers-and-position-1 ()
  "1の位置から３桁分105, 5の位置から2桁分13がある。"
  (should (equal '((1 3 103) (5 2 13))
                 (ndmacro-get-numbers-and-position
                  '(nil 49 48 51 nil 49 51 nil nil)))))

(ert-deftest ndmacro-get-incremented-sequence-1 () ; fail
    (equal '(49 48 53 44 49 55 65 return) ;"105,17A"
           (ndmacro-get-incremented-sequence
            '((return 65 53 49 44 52 48 49) ;"104,15A"
              (return 65 51 49 44 51 48 49) ;"103,13A"
              (return 65 49 49 44 50 48 49) ;"102,11A"
              ))))

(ert-run-tests-batch-and-exit)

(provide 'ndmacro-test)
;;; ndmacro-test.el ends here
