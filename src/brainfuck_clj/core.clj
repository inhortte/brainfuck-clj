(ns brainfuck-clj.core
  (:gen-class))

(def max-size 30000)
(defn reset-memoria! [m]
  (reset! m (byte-array (repeat max-size (byte 0)))))
(def memoria (atom (byte-array [(byte 0)])))
(reset-memoria! memoria)

(defn b-inc [v] (byte (if (= v (byte 127)) -128 (inc v))))
(defn b-dec [v] (byte (if (= v (byte -128)) 127 (dec v))))
(defn b-out [v] (char (if (< v 0) (+ v 256) v)))
(defn c-in [c] (let [v (int c)] (byte (if (> v 127) (- v 256) v))))

;; {:pointer 0 :program []}

(defn extract-loop [state]
  (loop [res []  [p & r] (:program state) depth 0]
    (case p
      \[ (recur (conj res p) r (inc depth))
      \] (if (zero? (dec depth)) (assoc state :program (rest res))
                    (recur (conj res p) r (dec depth)))
      (recur (if (> depth 0) (conj res p) res) r depth))))

(defn start-loop 
  "Note: only the pointer is updated in the return.
  It is up to the caller to drop the correct amount
  from program."
  [{:keys [pointer program] :as loop-state}]
  ;; (:pointeiprintln (str loop-state))
  (loop [inner-state loop-state]
    (if (empty? (:program inner-state))
      (if (zero? (aget @memoria (:pointer inner-state)))
        inner-state
        (recur loop-state))
      (recur (dispatch inner-state)))))

(defn inc-ptr [{:keys [pointer] :as state}]
  (if (< pointer max-size)
    (assoc state :pointer (inc pointer))
    (throw (Exception. "Reaching beyond the end of the brainfuck universe"))))

(defn dec-ptr [{:keys [pointer] :as state}]
  (if (> pointer 0)
	  (assoc state :pointer (dec pointer))
    (throw (Exception. "Stumbling backwards over zero"))))

(defn alter-cell [f pointer]
  (let [v (aget @memoria pointer)]
    (reset! memoria 
      (do (aset-byte @memoria pointer (f v))
          @memoria))))

(defn inc-cell [{:keys [pointer] :as state}]
  (alter-cell b-inc pointer)
  state) 

(defn dec-cell [{:keys [pointer] :as state}]
  (alter-cell b-dec pointer)
  state)

(defn put-char [state]
  (print (b-out (aget @memoria (:pointer state))))
  (flush)
  state)

(defn get-char [state]
  (alter-cell (fn [_]
                (c-in (first (str (read))))) (:pointer state))
  state)

(defn dispatch [{:keys [pointer program] :as state}]
  (let [[c & r] program]
    ;; (println (str "command: " c "  state: " (str state)))
    (case c
      \[ (let [loop-state (extract-loop state)]
           (assoc state :pointer (:pointer (start-loop loop-state)) :program (drop (+ 2 (count (:program loop-state))) (:program state))))
      (assoc
        (case c
          \> (inc-ptr state)
          \< (dec-ptr state)
          \+ (inc-cell state)
          \- (dec-cell state)
          \. (put-char state)
          \, (get-char state)
          state)
        :program (rest program)))))

(defn interpret [code]
  (loop [{:keys [program] :as state} {:pointer 0 :program code}]
    (if (empty? program)
      state
      (recur (dispatch state)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
