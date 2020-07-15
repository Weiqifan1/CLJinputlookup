
(load-file "src/clojurestat/hjaelpemetoder.clj")

(load-file "src/clojurestat/statistikfiler.clj")

(load-file "src/clojurestat/load_input.clj")


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

(defn regexInputSystem [inputCollection inputRegex] (filter #(re-matches (re-pattern (str inputRegex)) %) (keys inputCollection)))

;(defn userOutputFromLetters
;  [inputRegex systemLetter systemMap]
;  ((comp
;     (fn [inputlist]
;       (map
;         (fn [eachLettercharPair]
;           (if (get talmap (str (second eachLettercharPair)))
;             (merge-with + (hash-map :code (vector systemLetter (sort compareLength (get wubichartoletters (second eachLettercharPair)))));(first eachLettercharPair)))
;                         (get talmap (str (second eachLettercharPair))))
;             (hash-map
;               :code (vector systemLetter (sort compareLength (get wubichartoletters (second eachLettercharPair))));(first eachLettercharPair))
;               :char (second eachLettercharPair)
;               :ordinal (first (splitToCodepointVec (str (second eachLettercharPair))))
;               :cedicttrad false
;               :cedictsimp false
;               :tzai nil
;               :junda nil
;               )))
;         inputlist
;         ))
;     #(partition 2 %)
;     flatten
;     #(map systemMap %)
;     regexInputSystem
;     ) systemMap inputRegex))


(def chrMatches (filter #(re-matches (re-pattern (str "土.?")) (str (first %)))  wubichartoletters))
(def chrMatchCharList (map #(splitToCodepointChar (str (first %))) chrMatches))
(def chrMatchTalmapList (map (fn [input] (map #(get talmap %) input)) chrMatchCharList))
(def chrtegnSoegningRawInput (map (fn [foerste anden] (conj foerste anden)) chrMatches chrMatchTalmapList))

(defn charuserOutputNotSorted [rawUserOutput]
  (map-indexed
    (fn [idx itm]
      (str
        idx
        " " (nth itm 0);(get itm :char)
        " " (clojure.string/join " " (nth itm 1));(get itm :code)
        " "(clojure.string/join ", " (map #(
                                          str
                                          " char:" (get % :char)
                                          " junda:" (get % :junda)
                                          " tzai:" (get % :tzai)
                                          " ordinal:" (get % :ordinal)
                                          )
                                          (nth itm 2)))
        "\n"
        ))
    rawUserOutput))

(def testOutput (charuserOutputNotSorted chrtegnSoegningRawInput))

(defn userOutputFromChars
  [inputRegex systemLetter systemMap]

  (let [chrMatches (filter #(re-matches (re-pattern (str inputRegex)) (str (first %))) systemMap)
        splittetChars (map #(splitToCodepointChar (str (first %))) chrMatches)
        talmapMatches (map (fn [input] (map #(get talmap %) input)) splittetChars)
        rawResult (map (fn [foerste anden] (conj foerste anden)) chrMatches talmapMatches)
        ]
    (charuserOutputNotSorted rawResult)
    ))

;(println  (userOutputFromChars "土.?" "W" wubichartoletters))
;(println  (count (userOutputFromChars "土.?" "W" wubichartoletters)))
;(println  (type (take 1 (userOutputFromChars "土.?" "W" wubichartoletters))))
;(def entest (userOutputFromChars "土.?" "W" wubichartoletters))
;(println (first (chrMatchesFunc wubichartoletters "土.?")))

;slut