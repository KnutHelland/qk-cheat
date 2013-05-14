(ns qk-cheat.core
  (:require [clojure.java.io :as io]
            [clojure.data.json :as json])
  (:use [clojure.java.shell :only [sh]])
  (:import [java.net Socket HttpURLConnection URLEncoder]
           [java.io PrintWriter InputStreamReader BufferedReader]))

(def remember-token
  (str "BAhbB2kDJNgEIkUwNjJmNWQ3ZGY5OTYxYjBiMWMxMzU1MzZkMjNkZmIzMzdiN2IyNDYzOTR"
       "mZjNlNGI0YWIyMzgwMmNhODIwNDVh--cccad68bd606ad972c37fb1c4fb167d043521b69"))

;BAhbB2kDJNgEIkUwNjJmNWQ3ZGY5OTYxYjBiMWMxMzU1MzZkMjNkZmIzMzdiN2IyNDYzOTRmZjNlNGI0YWIyMzgwMmNhODIwNDVh--cccad68bd606ad972c37fb1c4fb167d043521b69

(defn urlencode
  [str]
  (URLEncoder/encode str "UTF-8"))

(defn params->string
  [params]
  (apply str
         (for [[k v] params] (str (name k) "=" (urlencode v) "&"))))

(defn curl
  [& params]
  (apply (partial sh "curl") (filter #(not (nil? %)) params)))

(defn request
  [method url params]
  (curl url
        "-H"
        (str "cookie: remember_token=" remember-token)
        "-X" (.toUpperCase method)
        (if (re-matches #"(?i)(post|put)" method)
          "--data"
          "")
        (if (re-matches #"(?i)(post|put)" method)
          (params->string params)
          "")
        "-k"))

(defn apirequest
  [method path params]
  (let [result (clojure.string/replace
    (:out (request method (str "https://qknorway.feomedia.se/" path) params))
    #"&quot;" "\"")]
    (try (json/read-str
          result
          :key-fn #(keyword %))
         (catch Exception e result))))

(defn get-user-id
  []
  (get-in
   (apirequest "post" "my_stats" {:some "yes"})
   '(:success
     :user_info
     :id)))

(defn get-my-turn-games
  [user-id]
  (filter
   #(and (:is_my_turn %) (= 1 (:game_state %)))
   (get-in
    (apirequest "post" (str "users/show_games/" user-id) {})
    '(:success
      :games
      :active_games))))

(defn get-active-games
  [user-id]
  (println "\n\n")
  (clojure.pprint/pprint (apirequest "post" (str
                                             "users/show_games/"
                                             user-id) {:some "yes"})))

(defn get-game
  "Takes a game id and loads the game."
  [game-id]
  (:success (apirequest "get" (str "qf_games/" game-id) {:some "yes"})))

(defn pick-category
  "Takes a game structure and returns a new list of categories with the
   new one."
  [game]
  (let [used (set (map #(Integer. %) (re-seq #"\d" (:categoryChoices game))))
        to-pick (atom (rand-int 6))]
    (while (contains? used @to-pick)
      (reset! to-pick (rand-int 6)))
    (str (apply str used) @to-pick)))


(defn new-category?
  [game]
  (>= (count (:my_answers game)) (* 3 (count (:categoryChoices game)))))

(defn do-answer
  [user-id game]
  (let [id (:game_id game)
        my-answers (str (:my_answers game) "000")
        category-choices (if (new-category? game)
                           (pick-category game)
                           (:categoryChoices game))]
    (apirequest "put" (str "qf_games/" id)
                {:game_update
                 (str "{\"categoryChoices\":\"" category-choices "\","
                      "\"game_state\":1,\"my_answers\":\"" my-answers "\","
                      "\"user_id\":\"" user-id "\"}")})))

(defn do-answer-all
  []
  (let [user-id (get-user-id)
        games (get-my-turn-games user-id)]
    (doall (for [game games]
             (do-answer user-id game)))))

(do-answer-all)

