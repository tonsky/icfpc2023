(ns icfpc2023.main
  (:require
    [cheshire.core :as json]
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [fipp.edn :as fipp]
    [icfpc2023.state :as state]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.protocols :as protocols]
    [io.github.humbleui.ui :as ui]
    [org.httpkit.client :as http])
  (:import
    [io.github.humbleui.jwm Window]
    [io.github.humbleui.jwm.skija LayerMetalSkija]
    [io.github.humbleui.skija ColorSpace]))

(defn read-response [res]
  (let [res @res]
    (prn res)
    (if (> (:status res) 299)
      (throw (ex-info (str (:status res) " " (:body res)) res))
      (json/parse-string (:body res) true))))

(defn fetch [n]
  (read-response
    (http/get (str "https://cdn.icfpcontest.com/problems/" n ".json"))))

(defn fetch-all
  ([] (fetch-all 1 3))
  ([to] (fetch-all 1 to))
  ([from to]
   (doseq [i (range 1 (inc to))]
     (with-open [w (io/writer (io/file "problems" (str i ".edn")))]
       (fipp/pprint (fetch i) {:writer w})))))

(comment
  (fetch 3)
  (fetch-all))

(defn submit [n]
  (read-response
    (http/post "https://api.icfpcontest.com/submission"
      {:headers {"Authorization" (str "Bearer " state/token)
                 "Content-Type" "application/json"}
       :body    (json/generate-string
                  {:problem_id n
                   :contents   (slurp (io/file "solutions" (str n ".json")))})})))

(comment
  (submit 1))

(def app
  (ui/default-theme
    (ui/center
      (ui/label "Hello, world"))))

(reset! state/*app app)

(add-watch state/*state ::redraw
  (fn [_ _ old new]
    (when (not= old new)
      (state/redraw))))

(reset! protocols/*debug? false)

(defn -main [& _args]
  (ui/start-app!
    (deliver state/*window
      (ui/window
        {:title    "ICFPC 2023"
         :mac-icon "resources/icon.icns"
         :bg-color 0xFFFFFFFF}
        state/*app)))
  (when (= :macos app/platform)
    (set! (.-_colorSpace ^LayerMetalSkija (.getLayer ^Window @state/*window)) (ColorSpace/getDisplayP3)))
  (state/redraw))
