
(load-file "src/clojurestat/hjaelpemetoder.clj")

(defn inputSystemFunc [filename lineRegex documentRegex]
  ((comp
     (fn [input] (map (fn [eachElem] (map #(clojure.string/lower-case %) eachElem)) input))
     ;herfra kan der vaere store bogstaver. disse skal vaere smaa
     (fn [input] (map #(vector (first %) (read-string (second %))) input))
     (fn [input] (map #(clojure.string/split % (re-pattern lineRegex)) input))
     #(clojure.string/split %  (re-pattern documentRegex))
     #(apply str (drop 1 (clojure.string/join "" (slurp %))))
     )
   filename))

;(println "************ wubi")
(def wubizizing (inputSystemFunc "src/clojurestat/WubiCodesCHR.txt" "\\s+" "\\n+"))
;(println (take 5 wubizizing))
;(println (count wubizizing))

;(println "************ array30")
;([CPU 溫] [WPU 媼] [G.; 啟] [FLVL 莠] [SLH 丸] [XXAX 幾] [OJQ, 爨] [, ，] [. 。] [; 口])
(def array30_1 (inputSystemFunc "src/clojurestat/array30_27489CHR.txt" "\\s+" "\\n+"))
(def array30_2 (inputSystemFunc "src/clojurestat/array30_ExtB.txt" "\\s+" "\\n+"))
(def array30_3 (inputSystemFunc "src/clojurestat/array30_ExtCD.txt" "\\s+" "\\n+"))

;(println (take 10 array30_1))
;(println (take 10 array30_2))
;(println (take 10 array30_3))
;(println (count array30_1))
;(println (count  array30_2))
;(println (count  array30_3))
;(println (type (first array30_1)))
;(println (type  (first array30_2)))
;(println (type (first array30_3)))
(def array30 (concat array30_1 array30_2 array30_3))
;(println (take 10 array30))
;(println (count array30))

;(println "********** cangjie ")

(defn inputSystemFuncEkstra [filename lineRegex documentRegex dropstring dropline]
   ((comp
       (fn [input] (map (fn [eachElem] (map #(clojure.string/lower-case %) eachElem)) input))
       ;herfra kan der vaere store bogstaver. disse skal vaere smaa
       (fn [input] (map #(clojure.string/split % (re-pattern lineRegex)) input))
       #(drop dropline %)
       #(clojure.string/split %  (re-pattern documentRegex))
       #(apply str (drop dropstring (clojure.string/join "" (slurp %))))
       )
    filename))

(def cangjie (inputSystemFuncEkstra "src/clojurestat/CangJie_textCHR.txt" "\\s+" "\\n+" 0 0))
;(println (take 5 cangjie))
;(println (count cangjie))

;(println "************************ dayi4_4code")
(def dayi4 (inputSystemFuncEkstra "src/clojurestat/dayi4.txt" "\\s+" "\\n+" 0 3))
;(println (take 50 dayi4))
;(println (count dayi4))

;(println "************************ dayi6_3code")
(def dayi6 (inputSystemFuncEkstra "src/clojurestat/dayi6_3code.txt" "\\s+" "\\n+" 0 1))
;(println (take 50 dayi6))
;(println (count dayi6))

(defn inputSystemWubihua [filename lineRegex documentRegex dropstring dropline]
  ((comp
     (fn [input] (map (fn [eachElem] (map #(clojure.string/lower-case %) eachElem)) input))
     ;herfra kan der vaere store bogstaver. disse skal vaere smaa
     ;([, 的] [,/nmm 的] [m 一] [n 乙] [, 人])
     (fn [input] (map #(vec (drop-last %)) input))
     ;([, 的 500] [,/nmm 的 500] [m 一 500] [n 乙 500] [, 人 495])
     (fn [input] (map #(clojure.string/split % (re-pattern lineRegex)) input))
     #(drop dropline %)
     #(clojure.string/split %  (re-pattern documentRegex))
     #(apply str (drop dropstring (clojure.string/join "" (slurp %))))
     )
   filename))

;(println "************************ wubihua")
(def wubihua (inputSystemWubihua "src/clojurestat/wubihua.txt" "\\s+" "\\n+" 0 155))
;(println (take 5 wubihua))
;(println (count wubihua))

(defn inputSystemFuncZhengma [filename lineRegex documentRegex dropstring dropline]
  ((comp
     (fn [input] (map (fn [eachElem] (map #(clojure.string/lower-case %) eachElem)) input))
     ;herfra kan der vaere store bogstaver. disse skal vaere smaa
    ;([a 一] [av 一] [aa 一下] [aaav 可歌可泣] [aacm 无可奉告])
     (fn [input] (map #(clojure.string/split % (re-pattern "=")) input))
     ;(a=一 av=一 aa=一下 aaav=可歌可泣 aacm=无可奉告)
     (fn [input] (map #(apply str %) input))
     ;((a = 一) (a v = 一) (a a = 一 下) (a a a v = 可 歌 可 泣) (a a c m = 无 可 奉 告))
     (fn [input] (map (fn [eachElem] (filter
                                       #(and (not= (.charAt "\"" 0) %)
                                             (not= (.charAt "<" 0) %)
                                             (not= (.charAt ">" 0) %)
                                             )
                                       eachElem)) input))
     ;("a"="一" "av"="一"> "aa"="一下" "aaav"="可歌可泣" "aacm"="无可奉告")
     #(apply concat %)
     (fn [input] (map #(clojure.string/split (first %) (re-pattern ",<")) input))
     ;(["a"="一",<"av"="一">] ["aa"="一下"] ["aaav"="可歌可泣"] ["aacm"="无可奉告"] ["aaez"="一开始"])
     (fn [input] (map #(clojure.string/split % (re-pattern lineRegex)) input))
     #(drop dropline %)
     #(clojure.string/split %  (re-pattern documentRegex))
     #(apply str (drop dropstring (clojure.string/join "" (slurp %))))
     )
   filename))

;(println "************************ zhengma")
(def zhengma (inputSystemFuncZhengma "src/clojurestat/zz201906_allcodes.txt" "\\s+" "\\n+" 0 92))
;(println (take 5 zhengma))
;(println (count zhengma))


;(println "**************** metoder der kan sendes til resten af programmet ******************")
;(def inputMethodEachLineMap (hash-map "W" wubizizing "A" array30 "C" cangjie "D" dayi4 "Y" dayi6 "H" wubihua "Z" zhengma))
;(def mychr (get inputMethodEachLineMap "Z"))
;(println (take 5 mychr))

(defn lettertopair [inputSystemLinePairs]
  ;wubilettertopair
  (let [wubiLetters (map first inputSystemLinePairs)
        ;wubichars (map second inputSystemLinePairs)
        cleanWubiVector (map (fn [letters] (filter #(= (first %) letters) inputSystemLinePairs)) wubiLetters)]
    (apply merge (map #(hash-map %1 %2)
                      wubiLetters cleanWubiVector
                      ))
    )
  )
;(println (take 5 (lettertopair zhengma)))

(defn chartoletters [inputSystemLinePairs]
  (let [wubichars (map second inputSystemLinePairs)
        cleanwubicharvector (map (fn [inputchars] (filter #(= (second %) inputchars) inputSystemLinePairs)) wubichars)
        wubichartopair (apply merge (map #(hash-map %1  %2)
                                         wubichars cleanwubicharvector
                                         ))]
    (apply merge ( map #(hash-map (first %)  (map first (second %))) wubichartopair))
    )
  )
;(println (take 5 (chartoletters zhengma)))

(defn theCapitalLetter [inputstring]
  (
    if (and (> (count inputstring) 0)
            (. java.lang.Character isLetter (first inputstring))
            (< (int (first inputstring)) 173)
            (= (first inputstring) (first (clojure.string/capitalize (first inputstring))))
            )
    (clojure.string/capitalize (first inputstring))
    nil
    )
  )

(defn hasJletter [inputstring]
  (if (and (> (count inputstring) 0)
           (.contains inputstring "J")
           )
    true
    false
    )
  )

(defn hasTletter [inputstring]
  (if (and (> (count inputstring) 0)
           (.contains inputstring "T")
           )
    true
    false
    )
  )

(defn theJorTletter [inputstring]
  (if (and (> (count inputstring) 0)
           (.contains inputstring "J")
           )
    compare-junda
    (if (and (> (count inputstring) 0)
             (.contains inputstring "T")
             )
      compare-tzai
      false
      )
    )
  )

;(println "hej der d")
;(println (hasJletter "hej derJ d"))
;(println (hasJletter "hej derE d"))

;(println "**************** metoder der kan sendes til resten af programmet ******************")
(def inputMethodEachLineMap
  (hash-map ;letter cant be J ot T. those are for sorting by junda or tsai
    "W" (hash-map "lettertopair" (lettertopair wubizizing) "chartoletters" (chartoletters wubizizing));wubizizing
    "A" (hash-map "lettertopair" (lettertopair array30) "chartoletters" (chartoletters array30));array30
    "C" (hash-map "lettertopair" (lettertopair cangjie) "chartoletters" (chartoletters cangjie));cangjie
    "D" (hash-map "lettertopair" (lettertopair dayi4) "chartoletters" (chartoletters dayi4));dayi4
    "Y" (hash-map "lettertopair" (lettertopair dayi6) "chartoletters" (chartoletters dayi6));dayi6
    "H" (hash-map "lettertopair" (lettertopair wubihua) "chartoletters" (chartoletters wubihua));wubihua
    "Z" (hash-map "lettertopair" (lettertopair zhengma) "chartoletters" (chartoletters zhengma));zhengma
    ))

;****************************** lav en funktion der gemmer dataen i et Atom (ved ikke om det vil virke) **********
;(def car
;  (atom {:make "Audi"
;         :model "Q3"}))
;;;;;;;;; inputMethodEachLineMap

;virker ikke
;(def newinputatom (atom {"newinputatom" inputMethodEachLineMap}))
;(println (take 5 (get (get (get newinputatom "newinputatom") "A") "lettertopair")))

;****************************** slut med at gemme i atomer


;slut