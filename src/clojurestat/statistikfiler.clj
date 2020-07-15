(load-file "src/clojurestat/hjaelpemetoder.clj")
(def filstier_stat (vector "src/clojurestat/Junda2005.txt" "src/clojurestat/Tzai2006.txt" "src/clojurestat/cedict_ts.txt"))

;************ hjaelpe metoder



;****************************



(defn clean_junda_funk [filstig regexSplitLinje regexSplitDokument]
  ((comp
     (fn [input] (map reverse (map #(vector (read-string (first %)) (second %)) input)))
     (fn [input] (map #(map clojure.string/trim %) input))
     (fn [input] (map #(clojure.string/split % (re-pattern regexSplitLinje)) input))
     #(clojure.string/split (slurp %) (re-pattern regexSplitDokument))
     )
   filstig
   )
  )

(def clean_junda (clean_junda_funk (get filstier_stat 0) "\\t+" "\\r+"))
(def jundavec (vec (set (map #(first %) clean_junda))))

(defn clean_tzai_funk [filstig regexSplitLinje regexSplitDokument]
  ((comp
     (fn [input] (map #(vector (first %) (read-string (second %))) input))
     (fn [input] (map #(clojure.string/split % (re-pattern regexSplitLinje)) input))
     #(map clojure.string/trim %)
     #(clojure.string/split (slurp %) (re-pattern regexSplitDokument))
     )
   filstig
   )
  )

(def clean_tzai (clean_tzai_funk (get filstier_stat 1) "\\s+" "\\r+"))
(def tzaivec (vec (set (map #(first %) clean_tzai))))

(def tzai_indexes (map-indexed (fn [idx itm] [(first itm) (inc idx)]) clean_tzai))
(def junda_hashmap (apply hash-map (vec (flatten clean_junda))))
(def tzai_hashmap (apply hash-map (vec (flatten tzai_indexes))))

(def cedict_raw_1 (clojure.string/split (slurp (get filstier_stat 2)) (re-pattern "\\r+")))
(def cedict_raw_1b (map clojure.string/trim cedict_raw_1))
(def cedict_raw_2 (map #(clojure.string/split % (re-pattern "\\s+")) cedict_raw_1b))
(def cedict_trad_raw (map #(filter-by-index % '(0)) cedict_raw_2))
(def cedict_simp_raw (map #(filter-by-index % '(1)) cedict_raw_2))
(def cedict_trad_string (clojure.string/join (flatten cedict_trad_raw)))
(def cedict_simp_string (clojure.string/join (flatten cedict_simp_raw)))
(def cedict_trad (set (splitToCodepointChar cedict_trad_string)))
(def cedict_simp (set (splitToCodepointChar cedict_simp_string)))

(def combivecInklAlpha (concat
                         jundavec ;(set jundaset)
                         tzaivec ;(set tzaiset)
                         cedict_trad ;(set cedict_trad)
                         cedict_simp ;(set cedict_simp)
                         ))

(def combivec (vec (set (filter #(not (and
                                        (> (first (splitToCodepointVec %)) 11904) ;65535)
                                        (< (first (splitToCodepointVec %)) 65535) ;65535)
                                        (not (. java.lang.Character isIdeographic (first (splitToCodepointVec %))))
                                        )) combivecInklAlpha))))




(def junda_tal (vec (map
                      #(if (get junda_hashmap (str %)) (get junda_hashmap (str %)) nil)
                      combivec)))
(def tzai_tal (vec (map
                     #(if (get tzai_hashmap (str %)) (get tzai_hashmap (str %)) nil)
                     combivec)))
(def cedictsimp_bool (vec (map
                            #(if (contains? cedict_simp %) true false)
                            combivec)))
(def cedicttrad_bool (vec (map
                            #(if (contains? cedict_trad %) true false)
                            combivec)))

(def talmap_values (map
                     #(hash-map :char %1 :ordinal (first (splitToCodepointVec %1)) :junda %2 :tzai %3 :cedictsimp %4 :cedicttrad %5)
                     combivec junda_tal tzai_tal cedictsimp_bool cedicttrad_bool
                     ))

;**********************************************
; "public" variables
;**********************************************

(def talmapWithSymbol (apply merge (map
                           #(hash-map (str %1) %2)
                           combivec talmap_values)))

;the line for the character 的 has a junda field of type clojure.lang.Symbol.
;this must be changed to Long
;[的 {:cedicttrad true, :tzai nil, :junda ﻿1, :ordinal 30340, :cedictsimp true, :char 的}]
(def talmap
  (assoc talmapWithSymbol
     "的" (hash-map :cedicttrad true, :tzai 1, :junda 1, :ordinal 30340, :cedictsimp true, :char "的")))

(comment

  (def neitherLongNorNil (filter
                           #(and
                              (not= java.lang.Long
                                    (type (get (val %) :junda)))

                              (not= nil ;java.lang.Long
                                    (type (get (val %) :junda)))
                              )
                           talmapWithSymbol))
  (println neitherLongNorNil)
  (println (type (get (val (first neitherLongNorNil)) :junda)))

  (println "hej statistik")
  (println (take 1 talmapWithSymbol))
  (println (val (first (take 1 talmapWithSymbol))))
  (println (get (val (first (take 1 talmapWithSymbol))) :char))
  (println (get (val (first (take 1 talmapWithSymbol))) :junda))
  (println (type (get (val (first (take 1 talmapWithSymbol))) :junda)))
  (println "")
  (println "test den nye talmap")
  ;(println  talmap)
  (println (get talmap "的"))
  (println (get (get talmap "的") :ordinal))
  (println (type (get (get talmap "的") :ordinal)))
  (println (get (get talmap "的") :junda))
  (println (type (get (get talmap "的") :junda)))
  (println (get (get talmap "的") :tzai))
  (println (type (get (get talmap "的") :tzai)))
  (println (count talmap))
  (println (count talmapWithSymbol))
  (println (filter
             #(and
                (not= java.lang.String
                      (type (get (val %) :char)))

                (not= nil ;java.lang.Long
                      (type (get (val %) :char)))
                )
             talmap))

  )
;slut