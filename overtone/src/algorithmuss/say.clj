(ns algorithmuss.say
  (:require [clj-http.client :as client])
  (:import (java.io File FileOutputStream ByteArrayInputStream)
           (javazoom.jl.player Player)))

(defn say [response]
  (let [mp3 (:body (client/get "http://translate.google.com/translate_tts"
                        {:query-params {"ie" "UTF-8"
                                        "tl" "en"
                                        "q" response}
                         :as :byte-array})) ]
    (with-open [player (new Player (ByteArrayInputStream. mp3))]
      (.play player))))
