(ns brainfuck-clj.core-spec
  (:require [speclj.core :refer :all]
            [brainfuck-clj.core :refer :all]))

(def lemur "++->>[+++--[<<..>.]-]++")

(describe "Loop extraction"
  (it "extracts the first loop from 'lemur'"
    (should= "+++--[<<..>.]-" (apply str (extract-loop {:program [lemur]})))))

(describe "functions excepting loops"
  (before-all (reset-memoria! memoria))
  (it "increments the pointer '>'"
    (should= 1 (:pointer (interpret ">"))))
  (it "decrements the pointer '<'"
    (should= 1 (:pointer (interpret ">>><<"))))
  (it "should throw an Exception '<'"
    (should-throw (interpret "<"))))

;; input two numbers, make them numbers : then multiply them (doesn't work)
;; ;; (interpret ">>>++++++[>++++++++[<<+>>-]<-]<<<,>>[<<->>-]>++++++[>++++++++[<<+>>-]<-]<<,>[<->-] now multiply <<[>[>+<-]<-]"))
