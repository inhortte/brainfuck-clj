(ns brainfuck-clj.core
  (:gen-class))

(def max-size 30000)
(def memoria (atom (byte-array (repeat max-size (byte 0)))))

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

(defn dispatch [{:keys [pointer program] :as state}]
  (let [[c & r] program]
    (case c
      \> (inc-ptr state))))

(defn interpret [code]
  (loop [{:keys [pointer program] :as state} {:pointer 0 :program code}]
    (if (empty? program)
      state
      (recur (dispatch state)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
