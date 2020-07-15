(ns clojurestat.core
(:require [org.httpkit.server :as server]
  [compojure.core :refer :all]
  [compojure.route :as route]
  [ring.middleware.defaults :refer :all]
  [clojure.pprint :as pp]
  [clojure.string :as str]
  [clojure.data.json :as json])
(:gen-class))

(load-file "src/clojurestat/hjaelpemetoder.clj")

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


(load-file "src/clojurestat/statistikfiler.clj")

(load-file "src/clojurestat/load_input.clj")

;(load-file "useroutput.clj")
(load-file "src/clojurestat/new_useroutput.clj")


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

;;;;;;;;;;;;;;;;;;;;;;;; her vil jeg lave rest
; Simple Body Page
(defn simple-body-page [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "Hello World"})

; request-example
(defn request-example [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (->>
              (pp/pprint req)
              (str "Request Object: " req))})

(defn hello-name [req] ;(3)
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (->
              (pp/pprint req)
              (str "Hello " (:name (:params req))))})

(defroutes app-routes
           (GET "/" [] simple-body-page)
           (GET "/request" [] request-example)
           (GET "/hello" [] hello-name)
           (route/not-found "Error, page not found!"))

(defn -main
  "This is our main entry point"
  [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "3000"))]
    ; Run the server with Ring.defaults middleware
    (server/run-server (wrap-defaults #'app-routes site-defaults) {:port port})
    ; Run the server without ring defaults
    ;(server/run-server #'app-routes {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))

;;;;;;;;;;;;;;;;;;;;;;;;rest er slut

;; jeg vil nu udkommentere mit loop
(comment
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
  )




; slut