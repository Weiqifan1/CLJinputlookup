(ns clojurestat.core)

(load-file "hjaelpemetoder.clj")

(defn sortUserOutput [x & args]
  (sort (first (flatten args)) x)
  )

(defn userOutputSorted [rawUserOutput & args]
  (map-indexed
    (fn [idx itm]
      (str
        idx
        " " (get itm :char)
        " " (get itm :code)
        " J:" (get itm :junda )
        " T:" (get itm :tzai)
        " CS:" (get itm :cedictsimp)
        " CT:" (get itm :cedicttrad)
        " O:" (get itm :ordinal)
        "\n"
        ))
    (sortUserOutput rawUserOutput args)))


;***************************** sorting functions end *********************


(defn regexInputSystem [inputCollection inputRegex] (filter #(re-matches (re-pattern (str inputRegex)) %) (keys inputCollection)))


(load-file "statistikfiler.clj")

(load-file "load_input.clj")

;(load-file "useroutput.clj")
(load-file "new_useroutput.clj")


;************************************** ----------------- ******************

;************************* user outputog pretty printing ********************************


;*************************** lav en skrive funktion der kan tage en regex og returnere user output **********



;*************************** lav grundlaeggende IO **********
(println "hello chr!")


;(if (stringHasChinese userInput)
;  (def sortedOutput (userOutputFromChars userInput "W" wubilettertopair))
;  (
;   (def rawOutput  (userOutputFromLetters userInput "W" wubilettertopair))  ; wubilettertopair   ((rhg 扯) (rhg 撦) (qhg 鉬) (qhg 钼)....
;   (def sortedOutput (vec (userOutputSorted (distinct rawOutput) compare-junda)))

;   )
;  )

;;;;;;;;;; her skal funktionen vaere 2020-06-14


(defn charOrLetterSearch
  [userInput systemLetterString jundaOrTzai]
  (
    if (stringHasChinese userInput)
    (userOutputFromChars userInput systemLetterString
                         ;wubichartoletters
                         (get (get inputMethodEachLineMap systemLetterString) "chartoletters")
                         )
    (vec (userOutputSorted (distinct(userOutputFromLetters userInput systemLetterString
                                                           ;wubilettertopair
                                                           ;(get (get inputMethodEachLineMap systemLetterString) "lettertopair")
                                                           )) jundaOrTzai));compare-junda))
    )
  )

(defn charOrLetterSearchAndDefault [userInput] ;jundaOrTzai
  ;brugerens input skal have formen EVTinputsystembogstav + EVTtradElSimp + kodeRegex,
  ; eks: jdf. Ajdf.  ATjdf.
  (if (not= (theCapitalLetter userInput) nil)
    ;(charOrLetterSearch   (apply str (drop 1 userInput))  (apply str (take 1 userInput)) compare-junda)
    (if (theJorTletter userInput)
      (charOrLetterSearch   (apply str (drop 2 userInput))  (apply str (take 1 userInput)) (theJorTletter userInput))
      (charOrLetterSearch   (apply str (drop 1 userInput))  (apply str (take 1 userInput)) compare-junda)
      )
    ;(charOrLetterSearch userInput "W" compare-junda)
    (if (theJorTletter userInput)
      (charOrLetterSearch (apply str (drop 1 userInput)) "W" (theJorTletter userInput))
      (charOrLetterSearch userInput "W" compare-junda)
      )
    )
  )

;************ backup
;(defn charOrLetterSearchAndDefault [userInput] ;jundaOrTzai
;  (if (not= (theCapitalLetter userInput) nil)
;    (charOrLetterSearch   (apply str (drop 1 userInput))  (apply str (take 1 userInput)) compare-junda)
;    ;((println (theCapitalLetter userInput))
;    ; (charOrLetterSearch (drop userInput) (str (theCapitalLetter userInput))))
;    (charOrLetterSearch userInput "W" compare-junda)
;    )
;  )
;****************



  (loop []
    (println "enter user regex (write \"*\" to quit)")
    (let [userInput (read-line)]
      (cond (not= userInput "*")
            (do
              ;********* programmet starter **********
              (def sortedOutput (charOrLetterSearchAndDefault userInput)) ;jundaOrTzai compare-junda or compare-tzai
              (def sortedSize (count sortedOutput))
              (if (< 10 sortedSize)
                (loop []
                  (println (str "laengden er: " sortedSize " skriv tal for at navigere"))
                  (let [navigation (read-line)]
                    (cond (and
                            (parse-int navigation)
                            (> (parse-int navigation) 0))

                          (do
                            (if (<= (+ sortedSize 1) (* 10 (parse-int navigation)))
                              (println (take-last 10 sortedOutput))
                              (println (subvec (vec sortedOutput) (- (* 10 (parse-int navigation)) 10) (* 10 (parse-int navigation))))
                              )
                            (recur))
                          :else
                          (println "back to regexes"))
                    )
                  )
                (println sortedOutput)
                )
              ;********* programmet slutter********* 我
              (recur))
            :else
            (println "quitting the program")
            )
      )
    )




; slut