(ns brainfuck-clj.core-spec
  (:require [speclj.core :refer :all]
            [brainfuck-clj.core :refer :all]))

(def lemur "++->>[+++--[<<..>.]-]++")

(describe "Loop extraction"
  (it "extracts the first loop from 'lemur'"
    (should= "+++--[<<..>.]-" (apply str (extract-loop {:program [lemur]})))))

(describe "functions excepting loops"
  (it "increments the pointer '>'"
    (should= 1 (:pointer (interpret ">")))))
