(ns connect4.core
  (:require [om.core :as om :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [cljs.core.async :as async :refer [<! >!]]
            [clojure.string :as string]
            [clojure.set :as set]
            [om-tools.dom :as dom :include-macros true]
            [goog.events :as events]
            [secretary.core :as secretary :include-macros true :refer [defroute]]
            )
  (:require-macros [cljs.core.async.macros :as m])
  (:import [goog.net XhrIo]
           [goog.events EventType])
  )

(enable-console-print!)

(def game (atom {:state {:current-turn :white
                         :board (vec (repeat 7 (vec (repeat 6 nil))))
                         :winner nil
                         }
                 :moves-chan (async/chan)
                 :id (.. js/window -gameConfig -gameId)
                 :controller ((comp keyword string/lower-case) (.. js/window -gameConfig -controller))
                 }))


(def colors {:white "#CB4B16" :black "#268BD2" nil "#eee8d5"})

(def config {:ncols 7 :nrows 6 :cellwidth 100})


(defcomponent circle [{:keys [c r color moves]} owner]
  (render [_]
                   (dom/circle
                    {:cx (+ 40 (* (:cellwidth config) c)) :cy (+ 50 (* 100 r)) :r 40 :fill color}))
  (display-name [_] "Circle")
  (did-mount [_]
     (events/listen (om/get-node owner) EventType.CLICK #(async/put! moves c))
       )
  )

(defn print-player-message [controller current-player]
  (dom/div {:style
             {:font-size "x-large"
              :text-align "center"
              :font-family "\"Comic Sans MS\", cursive, sans-serif"
              :color (current-player colors)
              :margin "auto"}}
           (prn controller)
           (prn current-player)
           (str (if (= controller current-player)
                  "Your"
                  "Opponent's"
                  ) " turn")
           )
  )

(defcomponent many-circles [data owner]
  (render [_]
          (dom/div {:style {:width (* 100 (:ncols config)) :margin "auto"}}
                   (dom/svg {:width (* 100 (:ncols config)) :height (* 100 (:nrows config))}
                            (for [c (range (:ncols config))
                                  r (range (:nrows config))
                                  :let [color (colors (get-in data [:state :board c r]))]
                                  ]
                              (->circle {:c c :r r :color color :moves (:moves-chan data)})
                              ))
                   (print-player-message (:controller data) (get-in data [:state :current-turn]))
                   )
          )
  (display-name [_] "Board")
  )

(defn fmaybe [f]
  (fn [arg?]
    (when-let [arg arg?]
      (f arg))
    )
  )

(defn str->cljgame [s]
  (do
    (let [json (.parse js/JSON s)
          cljs (set/rename-keys (js->clj json :keywordize-keys true) {:currentTurn :current-turn})]
      (-> cljs
          (update-in [:current-turn] (comp keyword string/lower-case))
          (update-in [:winner] (fmaybe #(keyword (.toLowerCase %))))
          (update-in [:board] (partial mapv (partial mapv (comp keyword (fmaybe string/lower-case)))))
          (update-in [:board] (partial mapv #(vec (concat (repeat (- (:nrows config) (count %)) nil) %))))
          )
      ))
  )

(defn cljgame->str [clj]
  (-> clj
      (set/rename-keys {:current-turn :currentTurn})
      (update-in [:currentTurn] (fmaybe (comp string/capitalize key->js)))
      (update-in [:winner] (fmaybe (comp string/capitalize key->js)))
      (clj->js)
      )
  )



(defn -read-game
  ([gid]
   (-read-game gid (async/chan))
   )
  ([gid ch] (.send XhrIo (str "rest/" gid) #(async/put! ch (str->cljgame (.. % -target getResponseText))))
   ch)
  )

(defn -send-move
  ([gid co move]
   (-send-move gid co move (async/chan))
   )
  ([gid co move ch]
   (.send XhrIo
              (str "rest/" gid "/" co)
              #(async/put! ch (str->cljgame (.. % -target getResponseText)))
              "POST"
              (str "[" move "]")
              )
   ch
   )
  )

(def read-game (partial -read-game (:id @game)))
(def send-move (partial -send-move (:id @game) ((comp string/capitalize name) (:controller @game))))

(m/go-loop [game-updates (async/chan)]
           (let [moves (:moves-chan @game)
                 reader (m/go
                         (<! (async/timeout 1000))
                         (<! (read-game))
                         )]
             (m/alt!
              reader ([g] (async/put! game-updates g))
              moves ([move] (send-move move game-updates))
              )
             (swap! game assoc-in [:state] (<! game-updates)))
           (recur (async/chan))
           )

@game

(om/root
  many-circles
  game
  {:target (. js/document (getElementById "app"))})
