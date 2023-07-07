(ns icfpc2023.main
  (:refer-clojure :exclude [any?])
  (:require
    [cheshire.core :as json]
    [clj-async-profiler.core :as profile]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.math :as math]
    [clojure.pprint :as pprint]
    [fipp.edn :as fipp]
    [icfpc2023.state :as state]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.protocols :as protocols]
    [io.github.humbleui.ui :as ui]
    [org.httpkit.client :as http])
  (:import
    [io.github.humbleui.jwm Window]
    [io.github.humbleui.jwm.skija LayerMetalSkija]
    [io.github.humbleui.skija Canvas Color ColorSpace Paint]))

(defn read-response [res]
  (let [res @res]
    (if (> (:status res) 299)
      (throw (ex-info (str (:status res) " " (:body res)) res))
      (json/parse-string (:body res) true))))

(defn fetch [n]
  (->
    (http/get (str "https://api.icfpcontest.com/problem?problem_id=" n))
    (read-response)
    :Success
    (json/parse-string true)))

(defn fetch-all
  ([] (fetch-all 1 45))
  ([to] (fetch-all 1 to))
  ([from to]
   (doseq [i (range 1 (inc to))]
     (println "Fetching" i)
     (with-open [w (io/writer (io/file "problems" (str i ".edn")))]
       (fipp/pprint (fetch i) {:writer w})))))

(comment
  (fetch-all))

(defn blocks? [x1 y1 x2 y2 x0 y0]
  (let [x1 (double x1)
        y1 (double y1)
        x2 (double x2)
        y2 (double y2)
        x0 (double x0)
        y0 (double y0)]
    (cond 
      (and (= x1 x0) (= y1 y0))
      false
    
      (< x0 (min x1 x2))
      false
    
      (> x0 (max x1 x2))
      false
    
      (< y0 (min y1 y2))
      false
    
      (> y0 (max y1 y2))
      false
    
      :else
      (let [a (- x2 x1)
            b (- y2 y1)
            c (* a (- y1 y0))
            d (* (- x1 x0) b)
            e (- c d)
            dist (/
                   (* (math/signum e) e)
                   (math/hypot a b))]
        (< dist 5.0)))))

(defn any? [pred xs]
  (reduce 
    (fn [_ x]
      (when (pred x)
        (reduced true)))
    nil xs))

(defn score [state mid attendee]
  (let [musician (nth (:placements state) mid)
        mx       (double (:x musician))
        my       (double (:y musician))
        ax       (double (:x attendee))
        ay       (double (:y attendee))]
    (if (any? #(blocks? mx my ax ay (:x %) (:y %)) (:placements state))
      0
      (let [instr    (nth (:musicians state) mid)
            taste    (nth (:tastes attendee) instr)]
        (math/ceil
          (/
            (* 1000000 taste)
            (+ (* (- mx ax) (- mx ax)) (* (- my ay) (- my ay)))))))))

(defn tastes [state]
  (->> (:attendees state)
    (reduce
      (fn [v a]
        (mapv + v (:tastes a)))
      (vec (repeat (count (:tastes (nth (:attendees state) 0))) 0)))
    (map vector (range))
    (into {})))

(defn shrink [percent xs]
  (filter (fn [_] (<= (rand) percent)) xs))

(defn places [state]
  (let [[left top] (:stage_bottom_left state)
        right      (+ left (:stage_width state))
        bottom     (+ top (:stage_height state))
        places     (loop [x      (+ left 10)
                          y      (+ top 10)
                          odd?   true
                          places []]
                     (cond
                       (> x (- right 10))
                       (recur (+ left (if odd? 20 10)) (+ y 17.33) (not odd?) places)
                       
                       (> y (- bottom 10))
                       places
                       
                       :else
                       (recur (+ x 20) y odd? (conj places {:x x :y y}))))
        attendees' (shrink 0.1 (:attendees state))]
    (->> places
      (pmap
        (fn [{:keys [x y] :as place}]
          (assoc place :visible
            (reduce
              (fn [acc attendee]
                (if (any? #(blocks? x y (:x attendee) (:y attendee) (:x %) (:y %)) places)
                  acc
                  (+ acc 1)))
              0 attendees'))))
      (sort-by :visible)
      reverse)))

(defn place-musicians [state]
  (let [tastes     (tastes state)
        places     (places state)
        musicians  (->> (mapv vector (range) (:musicians state))
                     (sort-by (fn [[id instr]]
                                (- (tastes instr))))
                     (mapv first))
        placements (loop [musicians  musicians
                          placements (vec (repeat (count musicians) nil))
                          places     places]
                     (cond
                       (empty? musicians)
                       placements
        
                       (empty? places)
                       (throw (ex-info "Not enough space!" {:placements placements}))
        
                       :else
                       (recur
                         (next musicians)
                         (assoc placements (first musicians) (first places))
                         (next places))))]
    (assoc state
      :placements placements)))

(defn score-musician [state mid]
  (let [{:keys [x y]} (nth (:placements state) mid)
        [score attendees'] (loop [score'     (long 0)
                                  attendees  (:attendees state)
                                  attendees' []]
                             (if (empty? attendees)
                               [score' attendees']
                               (let [attendee (first attendees)
                                     score''  (score state mid attendee)]
                                 (recur
                                   (long (+ score' score''))
                                   (next attendees)
                                   (conj attendees' (update attendee :score (fnil + 0) score''))))))]
    (-> state
      (update :score (fnil + 0) score)
      (update :placements update mid assoc :score score)
      (assoc :attendees attendees'))))

(defn submit [file]
  (read-response
    (http/post "https://api.icfpcontest.com/submission"
      {:headers {"Authorization" (str "Bearer " state/token)
                 "Content-Type" "application/json"}
       :body    (slurp file)})))

(defmacro measure [name & body]
  `(let [t# (System/currentTimeMillis)]
     ~@body
     (println ~name "took" (- (System/currentTimeMillis) t#) "ms")))

(defn set-problem! [i]
  (swap! state/*future
    #(do (some-> % future-cancel) nil))
  
  (reset! state/*future
    (future
      (try
        (measure (str "Problem " i)
          (reset! state/*state
            (assoc (edn/read-string (slurp (str "problems/" i ".edn")))
              :problem_id i))
          (swap! state/*state place-musicians)
          (profile/profile
            (doseq [i (range (count (:musicians @state/*state)))]
              (swap! state/*state score-musician i))))
        (catch Exception t
          (core/log-error t)))
      (reset! state/*future nil))))

(set-problem! 11)

(comment
  (doseq [i (range 7 46)]
    (try
      (measure (str "Problem " i)
        (reset! state/*state
          (assoc (edn/read-string (slurp (str "problems/" i ".edn")))
            :problem_id i))
        (swap! state/*state place-musicians)
        (doseq [i (range (count (:musicians @state/*state)))]
          (swap! state/*state score-musician i)))
      (let [state @state/*state]
        (let [file     (io/file (format "solutions/%02d%+016d.edn" i (:score state)))
              solution {:score      (:score state)
                        :placements (:placements state)
                        :attendees  (mapv #(select-keys % [:x :y :score]) (:attendees state))}]
          (with-open [w (io/writer file)]
            (fipp/pprint solution {:writer w})))
        (let [file     (io/file (format "solutions/%02d%+016d.json" i (:score state)))
              solution {:placements (mapv #(select-keys % [:x :y]) (:placements state))}
              contents (json/generate-string solution)
              body     (json/generate-string
                         {:problem_id i
                          :contents contents})]
          (spit file body)
          (when (pos? (:score state))
            (submit file))))
      (catch Exception t
        (core/log-error t))))
  
  (doseq [f (file-seq (io/file "solutions"))
          :let [name (.getName ^java.io.File f)]
          :when (str/ends-with? name ".json")
          :when (str/includes? name "+")]
    (println f)))

(defn draw-circles [circles canvas scale]
  (when circles
    (with-open [fill (paint/fill 0x40000000)]
      (let [min-score (transduce (keep :score) min 0 circles)
            max-score (transduce (keep :score) max 0 circles)]
        (doseq [{:keys [x y score]} circles]
          (cond
            (nil? score)
            (.setColor ^Paint fill (unchecked-int 0x40000000))
            
            (< -0.1 score 0.1)
            (.setColor ^Paint fill (unchecked-int 0xFFEEEEEE))
            
            :else
            (let [limit (if (>= score 0) max-score min-score)
                  base  (if (>= score 0) (unchecked-int 0xFF00FF00) (unchecked-int 0xFFFF0000))
                  alpha (-> score (/ limit) (* 205) (+ 50) (int))
                  color (Color/withA base alpha)]
              (.setColor ^Paint fill color)))
          (canvas/draw-circle canvas (* scale x) (* scale y) (max 3 (* scale 10)) fill))))))

(defn on-paint [ctx ^Canvas canvas size]
  (when-some [{:keys [problem_id stage_width stage_height room_width room_height attendees placements score]
               [stage_left stage_top] :stage_bottom_left} @state/*state]
    (let [min-x  (transduce (map :x) min stage_left attendees)
          min-y  (transduce (map :y) min stage_top attendees)
          max-x  (transduce (map :x) max (+ stage_left stage_width) attendees)
          max-y  (transduce (map :y) max (+ stage_top stage_height) attendees)
          width  (- max-x min-x)
          height (- max-y min-y)
          scale  (min
                   (/ (- (:width size) 100) width)
                   (/ (- (:height size) 100) height))
          left   (-> (:width size) (- (* width scale)) (/ 2) (- (* scale min-x)))
          top    (-> (:height size) (- (* height scale)) (/ 2) (- ( * scale min-y)))]
      (canvas/with-canvas canvas
        (canvas/translate canvas left top)
      
        (with-open [stroke (paint/stroke 0xFF000000 2)]
          ;; room
          (canvas/draw-rect canvas (core/rect-xywh 0 0 (* scale room_width) (* scale room_height)) stroke)
          ;; scene
          (canvas/draw-rect canvas (core/rect-xywh (* scale stage_left) (* scale stage_top) (* scale stage_width) (* scale stage_height)) stroke))
      
        ;; attendees
        (draw-circles attendees canvas scale)
      
        ;; musicians
        (draw-circles placements canvas scale))
    
      ;; score
      (canvas/draw-string canvas (str "Problem: " problem_id) 20 40 (:font-ui ctx) (:fill-text ctx))
      (canvas/draw-string canvas (format "Score: %,d" (or score -1)) 20 80 (:font-ui ctx) (:fill-text ctx)))))

(def app
  (ui/default-theme
    (ui/canvas
      {:on-paint #'on-paint})))

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
