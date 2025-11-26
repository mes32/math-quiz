#!/usr/bin/env bb

(require '[babashka.process :as p :refer [exec]])

(def NAME (or (System/getenv "PLAYER_NAME") "Player"))

(def NUMBER-OF-QUESTIONS 25)

(def QUESTION-TYPES [:addition :addition-3 :subtraction :bond])

(def WORDS-OF-ENCOURAGEMENT ["Correct!", "Thats right!", "You are correct!", "Yes!", "Good job!", "Winner winner chicken dinner!", "Awesome!", "Way to go!", "Yay!", "You go kiddo!"])

(def encouragement-cycle
  (cycle (shuffle WORDS-OF-ENCOURAGEMENT)))

(defn say [line]
  (p/process ["say" "-v" "Samantha" line]))

(defn print-bar []
  (println (apply str (repeat 48 "-"))))

(defn print-tabbed [line]
  (println (str "  " line)))

(defn title-greeting []
  (let [line (str "Hello " NAME ". Welcome to the Math Quiz!")]
    (say line)
    (print-bar)
    (println)
    (print-tabbed line)
    (println)
    (print-bar)))

(defn game-summary [score total-questions]
  (let [line (str "Quiz complete! You scored " score " out of " total-questions ".")]
    (Thread/sleep 2000)
    (say line)
    (println)
    (print-tabbed line)
    (println)
    (print-bar)))

#_(defn read-input []
  (flush) ; Ensure prompt is displayed before reading input
  (read-line))

(defn rand-in-range [start end]
  (+ start (rand-int (inc (- end start)))))

(defn gen-question []
  (let [type (rand-nth QUESTION-TYPES)]
  
  (case type
    :addition
    (let [num1 (rand-in-range 1 6)
          num2 (rand-in-range 0 7)]
      {:type type
       :prompt (str num1 " + " num2 " = ")
       :correct-answer (str (+ num1 num2))
       :correction-say (str num1 " plus " num2 " equals " (+ num1 num2))
       :correction-text (str num1 " + " num2 " = " (+ num1 num2))})

    :addition-3
    (let [num1 (rand-in-range 1 3)
          num2 (rand-in-range 1 2)
          num3 (rand-in-range 0 2)
          prompt (str num1 " + " num2 " + " num3 " = ")
          correct-answer (+ num1 num2 num3)]
      {:type type
       :prompt prompt
       :correct-answer (str correct-answer)
       :correction-say (str num1 " plus " num2 " plus " num3 " equals " correct-answer)
       :correction-text (str prompt correct-answer)})

    :subtraction
    (let [num1 (rand-in-range 1 10)
          num2 (rand-in-range 0 num1)]
      {:type type
       :prompt (str num1 " - " num2 " = ")
       :correct-answer (str (- num1 num2))
       :correction-say (str num1 " minus " num2 " equals " (- num1 num2))
       :correction-text (str num1 " - " num2 " = " (- num1 num2))})
       
    :bond
    (let [num1 (rand-in-range 1 2)
          num2 (rand-in-range 1 2)
          num3 (+ num1 num2)]
      {:type type
       :num1 num1
       :num2 num2
       :num3 num3
       :correct-answer (str num1)
       :correction-say (str "X equals " num1)
       :correction-text (str "X = " num1)}))))

(defn prompt-user! [question]
  (if (= :bond (:type question))
    (do (println)
        (println (str "     (" (:num3 question) ")"))
        (println (str "     / \\"))

        (println (str "    /   \\"))
        (println (str "  (X)   (" (:num2 question) ")"))
        (println)
        (print "  X = "))
    (do (println)
        (print (str "  " (:prompt question)))))
  (flush) ; Ensure prompt is displayed before reading input
  ;; TODO - loop while not numeric and trim user input string
  (read-line))

(defn evaluate-answer! [question answer current-score idx]
  (if (= (:correct-answer question) answer)
    (do (say (nth encouragement-cycle idx))
        (println)
        (print-tabbed "✅ Correct!")
        (print-bar)
        (inc current-score))
    (do (say (str "The correct answer is " (:correct-answer question) ". " (:correction-say question)))
        (println)
        (print-tabbed (str "❌ Incorrect. The correct answer is " (:correct-answer question) ". " (:correction-text question)))
        (print-bar)
        current-score)))

(defn game-loop []
  (loop [current-score 0
         idx 1]
    (if (> idx NUMBER-OF-QUESTIONS)
      current-score
      (let [question (gen-question)
            answer (prompt-user! question)
            next-score (evaluate-answer! question answer current-score idx)]
        (recur next-score (inc idx))))))

(defn run-game []
  (title-greeting)
  (let [score (game-loop)]
    (game-summary score NUMBER-OF-QUESTIONS)))

(run-game)
