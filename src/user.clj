(ns ^{:clojure.tools.namespace.repl/load false} user
  (:require
    [clojure.core.server :as server]
    [clojure.tools.namespace.repl :as ns]
    [icfpc2023.main :as main]
    [icfpc2023.state :as state]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.window :as window]))

(defn reset-window []
  (app/doui
    (when-some [window @state/*window]
      (window/set-window-position window 860 566)
      (window/set-content-size window 1422 800)
      #_(window/set-z-order window :floating))))

(defn reload []
  (let [res (ns/refresh :after 'icfpc2023.state/redraw)]
    (if (instance? Throwable res)
      (do
        (core/log-error res)
        (throw res))
      res)))

(defn -main [& args]
  (ns/set-refresh-dirs "src")
  (main/-main)
  #_(reset-window)
  (server/start-server
    {:name "repl"
     :port 5555
     :accept `server/repl
     :server-daemon false})
  (println "Started Socket Server on port 5555"))
