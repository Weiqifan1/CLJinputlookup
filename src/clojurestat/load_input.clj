
(load-file "src/clojurestat/hjaelpemetoder.clj")

(defn inputSystemFunc [filename lineRegex documentRegex]
  ((comp
     (fn [input] (map #(vector (first %) (read-string (second %))) input))
     (fn [input] (map #(clojure.string/split % (re-pattern lineRegex)) input))
     #(clojure.string/split %  (re-pattern documentRegex))
     #(apply str (drop 1 (clojure.string/join "" (slurp %))))
     )
   filename))

(def secondWubi (inputSystemFunc "src/clojurestat/WubiCodesCHR.txt" "\\s+" "\\n+"))

;(println (take 5 secondWubi))
;([a 工] [b 子] [c 又] [d 大] [e 月])

;/////////////////////
(def wubiLetters (map first secondWubi))
(def wubichars (map second secondWubi))
;////////////////////
(def cleanWubiVector (map (fn [letters] (filter #(= (first %) letters) secondWubi)) wubiLetters))

;///////////////////
(def cleanwubicharvector (map (fn [inputchars] (filter #(= (second %) inputchars) secondWubi)) wubichars))
(def wubichartopair (apply merge (map #(hash-map %1  %2)
                                      wubichars cleanwubicharvector
                                      )))

;************************************************************************
; "public" variables:
;************************************************************************

(def wubilettertopair (apply merge (map #(hash-map %1 %2)
                                        wubiLetters cleanWubiVector
                                        )))
;(println "wubilettertopair")
;(println (take 5 wubilettertopair))
;([qogl ([qogl 鰏])] [lkij ([lkij 加温])] [ncru ([ncru 以观后效])] [puaj ([puaj 襺])] [cwfw ([cwfw 圣伯夫])])

(def wubichartoletters  ( apply merge ( map #(hash-map (first %)  (map first (second %))) wubichartopair)))

;(println "wubichartoletters")
;(println (take 5 wubichartoletters))
;([嚥 (kau kld kauo)] [打破纪录 (rdxv)] [史馆 (kqqn)] [薡 (ahnh)] [要领 (svwy)])



;slut