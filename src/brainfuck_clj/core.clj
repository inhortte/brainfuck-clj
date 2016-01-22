(ns brainfuck-clj.core
  (:gen-class))

(def max-size 30000)
(def memoria (atom (byte-array (repeat max-size (byte 0)))))

(defn b-inc [v] (byte (if (= v (byte 127)) -128 (inc v))))
(defn b-dec [v] (byte (if (= v (byte -128)) 127 (dec v))))
(defn b-out [v] (char (if (< v 0) (+ v 256) v)))
(defn c-in [c] (let [v (int c)] (byte (if (> v 127) (- v 256) v))))

;; {:pointer 0 :program []}

(defn extract-loop [state]
  (loop [res []  [p & r] (peek (:program state)) depth 0]
    (case p
      \[ (recur (conj res p) r (inc depth))
      \] (if (zero? (dec depth)) (next res)
                    (recur (conj res p) r (dec depth)))
      (recur (if (> depth 0) (conj res p) res) r depth))))

(defn inc-ptr [{:keys [pointer program] :as state}]
  (if (< pointer max-size)
    (assoc state :pointer (inc pointer) :program (rest program))
    (throw (Error. "Reaching beyond the end of the brainfuck universe"))))

(defn dec-ptr [{:keys [pointer program] :as state}]
  (if (> pointer 0)
	(assoc state :pointer (dec pointer) :program (rest program))
    (throw (Error. "Stumbling backwards over zero"))))

(defn alter-cell [f pointer]
  (let [v (aget @memoria pointer)]
    (reset! memoria 
      (do (aset-byte @memoria pointer (f v))
          @memoria))))

(defn inc-cell [{:keys [pointer program] :as state}]
  (alter-cell b-inc pointer)
  (assoc state :program (rest program)))
  
(defn dec-cell [{:keys [pointer program] :as state}]
  (alter-cell b-dec pointer)
  (assoc state :program (rest program)))

(defn dispatch [{:keys [pointer program] :as state}]
  (let [[c & r] program]
    (case c
      \> (inc-ptr state)
      \< (dec-ptr state)
      \+ (inc-cell state)
      \- (dec-cell state)
      \. (do (print (b-out (aget @memoria pointer)))
            (flush)
            (assoc state :program (rest program)))
      \, (do (alter-cell (fn [_] (c-in (. System/in read))) pointer)
            (assoc state :program (rest program))))))

(defn interpret [code]
  (loop [{:keys [pointer program] :as state} {:pointer 0 :program code}]
    (if (empty? program)
      state
      (recur (dispatch state)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
