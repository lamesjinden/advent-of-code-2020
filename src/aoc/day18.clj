(ns aoc.day18
  (:require [clojure.string :as s]
            [instaparse.core :as insta])
  (:import (clojure.lang ExceptionInfo)))

(def day18-input-path "resources/input-day18.txt")

(def sample-input1 "1 + 2 * 3 + 4 * 5 + 6")                 ;71
(def sample-input2 "2 * 3 + (4 * 5)")                       ;26
(def sample-input3 "5 + (8 * 3 + 9 + 3 * 4 * 3)")           ;437
(def sample-input4 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") ;12240
(def sample-input5 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") ;13632

(def p1-parser
  (insta/parser
    "expression = expression <#'\\s'> operator <#'\\s'> term | term
    term = #'\\d+' | <'('> expression <')'>
    operator = '+' | '*'"))

(defn str->expression [parser str]
  (let [parsed (parser str)]
    (if (insta/failure? parsed)
      (throw (ex-info "Parse Failure" {:str     str
                                       :failure (insta/get-failure parsed)}))
      parsed)))

(defn input->expressions [parser input-path]
  (->> input-path
       (slurp)
       (s/split-lines)
       (map #(str->expression parser %))))

(defn evaluate-expression [[_
                            [l-type l-value :as l-ex]
                            [_ op-value]
                            [_r-type r-value]]]
  (let [str->op #(case % "+" + "*" * +)
        result (case l-type
                 :term (let [left (cond
                                    (string? l-value) (Long/parseLong l-value)
                                    (vector? l-value) (evaluate-expression l-value))
                             op (str->op op-value)
                             right (cond
                                     (string? r-value) (Long/parseLong r-value)
                                     (vector? r-value) (evaluate-expression r-value)
                                     :else 0)]
                         (op left right))
                 :expression (let [left (evaluate-expression l-ex)
                                   op (str->op op-value)
                                   right (cond
                                           (string? r-value) (Long/parseLong r-value)
                                           (vector? r-value) (evaluate-expression r-value)
                                           :else 0)]
                               (op left right)))]
    result))

(defn part1
  ([]
   (part1 (doall (input->expressions p1-parser day18-input-path))))
  ([expressions]
   (try
     (->> expressions
          (map evaluate-expression)
          (reduce +))
     (catch ExceptionInfo e
       (prn "Ex-Info" e)))))

(def p2-parser
  (insta/parser
    "expression = term <#'\\s'> mul <#'\\s'> expression | term
    term = factor <#'\\s'> add <#'\\s'> term | factor
    factor = '(' expression ')' | const
    const = #'\\d+'
    add = '+'
    mul = '*'"))

(defn evaluate-expression2 [[type
                             l-token
                             m-token
                             r-token]]

  (let [result (case type
                 :const (Long/parseLong l-token)
                 :factor (if (= "(" l-token)
                           (evaluate-expression2 m-token)
                           (evaluate-expression2 l-token))
                 :term (let [left (evaluate-expression2 l-token)
                             right (if (nil? r-token)
                                     0
                                     (evaluate-expression2 r-token))]
                         (+ left right))
                 :expression (let [left (evaluate-expression2 l-token)
                                   right (if (nil? r-token)
                                           1
                                           (evaluate-expression2 r-token))]
                               (* left right)))]
    result))

(defn part2
  ([]
   (part2 (input->expressions p2-parser day18-input-path)))
  ([expressions]
   (try
     (->> expressions
          (map evaluate-expression2)
          (reduce +))
     (catch ExceptionInfo e
       (prn "Ex-Info" e)))))
