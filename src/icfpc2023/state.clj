(ns ^{:clojure.tools.namespace.repl/load false}
  icfpc2023.state
  (:require
    [io.github.humbleui.window :as window]))

(def *app
  (atom nil))

(def *window
  (promise))

(def *state
  (atom nil))

(defn redraw []
  (some-> *window deref window/request-frame)
  :success)

(def token
  (System/getenv "ICFPC2023_TOKEN"))