;;; ndmacro.el --- macro that support counting -*- lexical-binding: t; -*-

;; Copyright (C) 2012 snj14

;; Author: snj14 <esnj14@gmail.com>>
;; Maintainer: ril <fenril.nh@gmail.com>
;; Version: 0.1.0
;; Keywords: convenience
;; URL: https://github.com/fenril058/ndmacro.el
;; Package-Requires: ((emacs "24.3"))

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Usage:
;;   (require 'ndmacro)
;;   (global-set-key (kbd "C-t") 'ndmacro)
;;
;; Or, with use-package:
;;   (use-package ndmacro
;;     :bind ("C-t" . ndmacro))
;;
;; Or, wiht leaf:
;;   (leaf ndmacro
;;     :hook ("C-t" . ndmacro))
;;
;; Related cod:
;; * dmacro.el (original version)
;;   written by 増井俊之 <masui@ptiecan.com> & 太和田誠 on 1993-03-14
;;   and extented for XEmacs by 小畑英司 & 峰伸行 on 2022-03
;;   <http://www.pitecan.com/papers/JSSSTDmacro/dmacro.el>
;;
;; * dmacro.el (emacs-jp version)
;;   Import the original version on 2017-10-21
;;   and developed and maintained by USAMI Kenta <tadsan@zonu.me>
;;   <https://github.com/emacs-jp/dmacro>
;;
;; * ndmacro.l
;;   written by kia on 2003-06-30
;;   <https://web.archive.org/web/20190330074136/http://www.geocities.jp/kiaswebsite/xyzzy/ndmacro.l.txt>
;;
;; * ndmacro.el
;;   written by snj14 on 2012-02-08
;;   <https://github.com/snj14/ndmacro.el>
;;



;;; Code:

(require 'cl-lib)

(defgroup ndmacro nil "New Dynamic Macro"
  :group 'convenient
  :prefix "ndmacro-")

(defvar ndmacro-repeat-count 0)

;; from On Lisp utility
(defun ndmacro-util-group (source n)
  "List SOURCEをN個ずつ分けたlistにする。"
  (if (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons
                                 (cl-subseq source 0 n)
                                 acc))
                    (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))

(defun ndmacro-list-shift (list1 list2)
  "LIST1の最後の要素をLIST2の先頭の要素にしたリストを返す。"
  (list (reverse (cdr (reverse list1)))
        (cons (car (reverse list1)) list2)))

(defun ndmacro-is-number (x)
  "XがASCIIで数字に該当しないときnilをそれ以外はX自身を返す。"
  (if (and (numberp x)
           (<= 48 x)
           (<= x 57))
      x nil))

(defun ndmacro-is-not-number (x)
  "XがASCIIで数字に該当するときnilをそれ以外はX自身を返す。"
  (if (and (numberp x)
           (<= 48 x)
           (<= x 57))
      nil x))

(defun ndmacro-seq-prefix-matched (lst1 lst2)
  "LST1の末尾をLST2の先頭に移動した2つのlistを要素に持つlistを返す。"
  (let ((idx 0))
    (message "-1:%s" lst1)
    (message "-2:%s" lst2)
    (while (equal (nth idx lst1)
                  (nth idx lst2))
      ;; (message "-3:%s" lst1)
      ;; (message "-4:%s" lst2)
      (cl-incf idx))
    (cl-subseq lst1 0 idx)))

(defun ndmacro-search-loop (lst)
  "lstを半分にして、前半分と後半分が先頭一致するか見る。
一致しなければ前半分の一番後ろを後半分の先頭に持ってきて比較。以降繰り返し。"
  (let* ((center-pos (floor (length lst) 2))
         (list1 (cl-subseq lst 0 center-pos))
         (list2 (cl-subseq lst center-pos))
         shifted)
    ;; 数字[0-9]同士は同じものとみなす。あとから差を出して連番生成できるように。
    (setq list1 (mapcar 'ndmacro-is-not-number list1))
    (setq list2 (mapcar 'ndmacro-is-not-number list2))

    (while (and list1 ;; list1が残ってれば続ける
                ;; list1,list2が異なれば続ける
                (not (equal list1
                            (cl-subseq list2 0 (length list1)))))
      (setq shifted (ndmacro-list-shift list1 list2)
            list1   (car  shifted)
            list2   (cadr shifted))
      )
    ;; ループが終わったら,list1は｛空っぽ｜list2との先頭一致リスト｝のどっちか。

    (cond (list1
           (let ((begin 0)
                 (end   (length list1)))
             (while (equal list1 (cl-subseq list2 begin (min (length list2) end)))
               (setq begin (+ begin (length list1))
                     end   (+ end   (length list1))))
             ;; 繰り返しの全体と、途中までの場合何桁目まで入力しているか？を返す
             (cl-values (ndmacro-util-group (cl-subseq lst 0 (min (length lst) (+ end (length list1))))
                                            (length list1))
                        0)))
          (t
           ;; 完全一致の繰り返しがなければ途中までの入力から予測
           (ndmacro-predict-repeat lst)))))

(defun ndmacro-predict-repeat (lst)
  (let* ((lst lst)                      ; --time-->
         (latest-val-pos (cl-position (cl-first lst) lst :start 1))
         repeat-start-pos
         repeat-end-pos)
    (if (null latest-val-pos)
        (user-error "There seems to be no repetitive operation.")
      (setq repeat-end-pos (length (ndmacro-seq-prefix-matched
                                    (cl-subseq lst 0 latest-val-pos)
                                    (cl-subseq lst latest-val-pos))))
      (setq repeat-start-pos (+ latest-val-pos
                                repeat-end-pos))
      (cons (list (cl-subseq lst
                             repeat-end-pos
                             repeat-start-pos)
                  (append (cl-subseq lst repeat-end-pos latest-val-pos)
                          (cl-subseq lst 0 repeat-end-pos)))
            (list repeat-end-pos)))
    ))

(defun ndmacro-split-seq-if (test lst)
  (let (beg end)
    (when (setq beg (cl-position-if     test lst :start 0))
      (setq end (or (cl-position-if-not test lst :start beg) (length lst)))
      (cons (cl-subseq lst beg end)
            (ndmacro-split-seq-if test (cl-subseq lst end))))))

(defun ndmacro-position-subseq (lst sub)
  (let ((pos 0)
        (continue-flag t)
        res)
    (while (and continue-flag
                (setq pos (cl-position (cl-first sub) lst :start pos)))
      (cond ((equal (cl-subseq lst pos (+ pos (length sub)))
                    sub)
             (setq res pos)
             (setq continue-flag nil))
            (t
             (cl-incf pos))))
    res))

(defun ndmacro-get-numbers-and-position (lst)
  (let* ((splitted (ndmacro-split-seq-if 'identity lst))
         (numbers (mapcar (lambda (l)
                            (apply 'cl-concatenate 'string
                                   (mapcar 'string l)))
                          splitted)))
    (cl-mapcar 'list
               (mapcar #'(lambda (sub) (ndmacro-position-subseq lst sub))
                       splitted)
               (mapcar #'(lambda (n) (length n)) numbers)
               (mapcar 'string-to-number numbers))))

(defun ndmacro-get-incremented-sequence (lst)
  (setq lst (mapcar 'reverse lst))
  (let* (;; 数字以外nilに変えちゃう
         (lst1  (ndmacro-get-numbers-and-position
                 (mapcar 'ndmacro-is-number (nth 0 lst))))
         (lst2 (ndmacro-get-numbers-and-position
                (mapcar 'ndmacro-is-number (nth 1 lst))))
         (next-number
          (cl-mapcar 'list ; 位置情報もくっつけとく。
                     lst1
                     (cl-mapcar '+ ; 足すと次の数字になって↑↑
                                (mapcar 'third lst1)
                                (mapcar (lambda (e)
                                          (* ndmacro-repeat-count e)) ; 連続実行の場合は実行回数をかけて↑↑
                                        (cl-mapcar '- ; 差を出して↑↑
                                                   (mapcar 'third lst1)
                                                   (mapcar 'third lst2)))
                                )))
         (result-seq (cl-copy-list (car lst))))
    (dolist (l next-number) ;;繰り返し1つの中に複数数字がある場合に備えて
      (let ((chars (cl-map 'list
                           'identity
                           (substring (format "000000000000000000%d" (max 0 (cadr l))) ;;桁数維持
                                      (- (cadar l))))))
        (dotimes (n (cadar l))
          (setf (nth (+ n (caar l)) result-seq) (nth n chars)))))

    result-seq ;; ←これが連番の増えたver
    ))

(defun ndmacro-get-key-list ()
  (let ((lst (reverse (append (recent-keys) nil)))
        loop-elm loop-all input-count result match-pos)
    ;; 繰り返しとみなさないものを除外：
    ;; 直近のndmacroキーを除外した上で、
    (while (and (setq match-pos (cl-position last-command-event lst :test 'equal))
                (= match-pos 0))
      (setq lst (cdr lst)))
    ;; 最後にndmacroキーを押した時以降の入力を探索対象に。
    ;; => ndmacroキーを跨いで繰り返しとみなさない
    (setq lst (cl-subseq lst 0
                         (cl-position last-command-event lst :test 'equal)))
    ;; 繰り返しを探す
    (cl-multiple-value-setq (loop-all input-count)
      (ndmacro-search-loop lst))

    (setq loop-elm (reverse (nth -1 loop-all)))

    ;;数字が入ってたら連番増やす
    (setq result (cond ((cl-find-if 'ndmacro-is-number loop-elm)
                        ;; (message "%s" loop-all)
                        (ndmacro-get-incremented-sequence loop-all))
                       (t
                        loop-elm)))
    ;;繰り返しを予測した場合の最初のndmacroキーの時のみ繰り返し要素の一部のみ実行
    (cond ((and (= ndmacro-repeat-count 1)
                (< 0 input-count))
           (nthcdr input-count result))
          (t result))))

;;;###autoload
(defun ndmacro ()
  "キー操作の繰返しを検出し実行する。数字の場合は連番を生成する。"
  (interactive)
  (cond ((equal real-last-command this-command)
         (cl-incf ndmacro-repeat-count))
        (t
         (setq ndmacro-repeat-count 1)))
  ;; (message "lc:%s tc:%s lce:%c tck:%s lcc:%c lie:%c lef:%s"
  ;;                real-last-command this-command last-command-event
  ;;                (this-command-keys)
  ;;                last-command-char
  ;;                last-input-event
  ;;                last-event-frame
  ;;                )
  (let ((lst (ndmacro-get-key-list)))
    (cond ((not lst)
           (user-error "繰り返しが見つかりません。"))
          ((not nil)
           ;; (message "繰り返しはこれです：%s" lst)
           (execute-kbd-macro (apply 'vector lst))))))

;;; TODO:
;;;
;;; - win, macで確認すること
;;; - dmacroの規則２ (XYXのパターン)に対応させる?

(provide 'ndmacro)
;;; ndmacro.el ends here
