(ns user
  (:use [figwheel-sidecar.repl-api :as fw]))

(defn start []
  (fw/start-figwheel!)
  (fw/cljs-repl "dev"))

(defn stop []
  (fw/stop-figwheel!))
