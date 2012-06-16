(defproject quora "0.1.0"
  :repositories {"stuart" "http://stuartsierra.com/maven2"}
  :description "Quora hiring \"challenges\""
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :dev-dependencies [[midje "1.4.0"]
                     [com.stuartsierra/lazytest "1.2.3"
                      :exclusions [swank-clojure]]])