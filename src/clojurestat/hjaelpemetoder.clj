
(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn parse-int
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^\d+$" s);(if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(defn filter-by-index [coll idxs]
  (keep-indexed #(when ((set idxs) %1) %2)
                coll))
(defn parse-long [s]
  (Long. (re-find #"\d+" s)))

(defn splitToCodepointChar
  ;"A腦𡳞𨨏B乃匯C𨭎" [A 腦 𡳞 𨨏 B 乃 匯 C 𨭎]
  [kinstring]
  ((comp
     #(clojure.string/split % #" ")
     #(clojure.string/join "" %)
     (fn [x] (map
               #(if
                  (and (>= (int %) 55296) (<= (int %) 56319))
                  % (str % " ")) x))
     ) kinstring))

(defn splitToSurogatePair
  ;"A腦𡳞𨨏B乃匯C𨭎" ((65) (33126) (55367 56542) (55394 56847) (66) (20035) (21295) (67) (55394 57166))
  [kinstring]
  ((comp
     (fn [x] (map #(map int %) x))
     #(splitToCodepointChar %)
     ) kinstring))

(defn splitToCodepointVec
  ;"A腦𡳞𨨏B乃匯C𨭎" (65 33126 138462 166415 66 20035 21295 67 166734)
  [nested]
  (map
    #(if
       (and (>= (first (map int %)) 55296) (<= (first (map int %)) 56319))
       (. java.lang.Character toCodePoint (first %) (second %))
       (first (map int %)))
    (splitToCodepointChar
      nested)))

(defn stringHasChinese [inputstring]
  (< 0
    (count (filter #(> % 11000) (splitToCodepointVec inputstring)))
  );hvis over 11.000 = ikke kinesisk
  )

(defn compareLength
  [a b] (
          if (= (count a) (count b))
          (compare a b)
          (if (and (= (count a) 1) (> (count b) 1))
            -1
            (if (and (= (count b) 1) (> (count a) 1))
              1
              (if (> (count a) (count b))
                -1
                1
                )
              )
            )
          ))

;***************************** sorting functions ***************************
(defn compare-tzai-secondary [a b]
  (if (and (= (get a :tzai) nil) (not= (get b :tzai) nil))
    1
    (if (and (= (get b :tzai) nil) (not= (get a :tzai) nil))
      -1
      (if (and (not= (get a :tzai) nil) (not= (get b :tzai) nil))
        (compare (get a :tzai) (get b :tzai))
        (compare (get a :ordinal) (get b :ordinal))
        )
      )
    )
  )

(defn compare-junda-secondary [a b]
  ;(sort-by (juxt :junda :tzai :cedictsimp :cedicttrad :ordinal) x)
  (if (and (= (get a :junda) nil) (not= (get b :junda) nil))
    1
    (if (and (= (get b :junda) nil) (not= (get a :junda) nil))
      -1
      (if (and (not= (get a :junda) nil) (not= (get b :junda) nil))
        (compare (get a :junda) (get b :junda))
        (compare (get a :ordinal) (get b :ordinal));(compare (get a :tzai) (get b :tzai))
        )
      )
    )
  )

(defn compare-tzai [a b]
  (if (and (= (get a :tzai) nil) (not= (get b :tzai) nil))
    1
    (if (and (= (get b :tzai) nil) (not= (get a :tzai) nil))
      -1
      (if (and (not= (get a :tzai) nil) (not= (get b :tzai) nil))
        (compare (get a :tzai) (get b :tzai))
        (compare-junda-secondary a b)
        )
      )
    )
  )

(defn compare-junda [a b]
  ;(sort-by (juxt :junda :tzai :cedictsimp :cedicttrad :ordinal) x)
  (if (and (= (get a :junda) nil) (not= (get b :junda) nil))
    1
    (if (and (= (get b :junda) nil) (not= (get a :junda) nil))
      -1
      (if (and (not= (get a :junda) nil) (not= (get b :junda) nil))
        (compare (get a :junda) (get b :junda))
        (compare-tzai-secondary a b);(compare (get a :tzai) (get b :tzai))
        )
      )
    )
  )