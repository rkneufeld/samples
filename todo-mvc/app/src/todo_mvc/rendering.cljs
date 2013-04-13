(ns todo-mvc.rendering
  (:require [domina :as dom]
            [domina.xpath :as dom-xpath]
            [domina.events :as dom-events]
            [domina.css :as css]
            [io.pedestal.app.protocols :as p]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.render.push.templates :as t]
            [io.pedestal.app.render.push.handlers.automatic :as d]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.util.log :as log])
  (:require-macros [todo-mvc.html-templates :as html-templates]))

(def templates (html-templates/todo-mvc-templates))

(defn render-simple-page [renderer [_ path _] transmitter]
  (let [parent (render/get-id renderer path)
        html (t/add-template renderer path (:todo-mvc-page templates))]
    (dom/append! (dom/by-id parent) (html {}))))

(def ^:private enter-key 13)
(declare add-todo-handler)
(defn enable-todo-transforms [renderer [_ path transform msgs] transmitter]
  (condp = transform
    :add-todo (dom-events/listen! (dom/by-id "new-todo")
                                  :keyup
                                  (add-todo-handler transform msgs transmitter))
    :clear-completed (dom-events/listen! (dom/by-id "clear-completed")
                                         :click
                                         (clear-completed-todos-handler transform msgs transmitter))
    (log/error :in :enable-todo-transforms :unmatched transform)))

(defn add-todo-handler [transform-name original-messages transmitter]
  (fn [e]
    (if (= (:keyCode e) enter-key)
      (let [content (-> "new-todo"
                        dom/by-id
                        dom/value)
            messages (msg/fill transform-name original-messages {:content content})]
        (doseq [msg messages]
          (p/put-message transmitter msg))))))

(defn clear-completed-todos-handler [transform-name original-messages transmitter]
  (fn [e]
   (let [messages (msg/fill transform-name original-messages)]
     (doseq [msg messages]
       (p/put-message transmitter msg)))))

(defn classify-element [new-selected]
  (dom/remove-class! (dom-xpath/xpath "//ul[@id='filters']/li/a") "selected")
  (dom/set-classes! (dom/by-id new-selected) "selected"))

(defn filter-handler [transform-name original-messages transmitter]
  (fn [e]
    (classify-element (-> e .-evt .-target .-id))
    (let [messages (msg/fill transform-name original-messages)]
      (doseq [msg messages]
        (p/put-message transmitter msg)))))

(defn enable-filter-transforms [renderer [_ path transform msgs] transmitter]
  (let [handler-fn (filter-handler transform msgs transmitter)]
    (condp = transform
      :view-all       (dom-events/listen! (dom/by-id "all")       :click handler-fn)
      :view-active    (dom-events/listen! (dom/by-id "active")    :click handler-fn)
      :view-completed (dom-events/listen! (dom/by-id "completed") :click handler-fn)
      (log/error :in :enable-filter-transforms :unmatched transform))))

(defn id->int [id]
  (->> id
       (re-find #"[0-9]+")
       js/parseInt))

(defn all-todo-ids []
  (let [nodes (dom/nodes (css/sel "#todo-list div.view"))
        node->id (comp :id dom/attrs)]
    (map node->id nodes)))

(defn get-prependable-id [todo-id]
  (let [int-id-map (into {} (map (fn [x] [(id->int x) x]) (all-todo-ids)))
        ints       (sort (keys int-id-map))
        matchable-int    (id->int todo-id)
        next-highest-int (first (drop-while #(< % matchable-int) ints))
        prependable-id   (int-id-map next-highest-int)]
    prependable-id))

(defn get-insertion-fn [todo-id]
  (if-let [prependable-id (get-prependable-id todo-id)]
    (partial dom/insert-before! (.-parentNode (dom/by-id prependable-id)))
    (partial dom/append!        (dom/by-id "todo-list"))))

(defn create-todo-item [renderer [event path old new] transmitter]
  ;; At the moment this ID attachs to the first child of our todo li,
  ;; on account of a limitation whereby you cannot set both another
  ;; field AND id on a template.
  (when new
    (let [id (render/new-id! renderer path (:uuid new))
          insertion-fn (get-insertion-fn id)
          html (t/add-template renderer path (:todo-item templates))
          todo-map (assoc new :id id)]
      (insertion-fn (html todo-map))
      (if (:completed? todo-map)
        (let [checkbox-sel (str "//*[@id='" id "']//input[@type='checkbox']")
              checkbox (dom-xpath/xpath checkbox-sel)]
          (dom/set-attr! checkbox "checked" "checked"))))))

(def event-to-class
  {:toggle-complete "toggle"
   :delete-todo "destroy"})

(defn create-todo-item-event [renderer [_ path event msgs] transmitter]
  (let [id (render/get-id renderer path)]
    (doseq [msg msgs]
      (let [toggle-selector (str "//*[@id='" id "']//*[@class='" (event-to-class event) "']")]
        (dom-events/listen! (dom-xpath/xpath toggle-selector)
                            :click
                            (fn [_] (p/put-message transmitter msg)))))))

(defn disable-clear-completed-event [renderer [_ path event msgs] transmitter]
  (let [selector (str "//*[@id='clear-completed']")]
    (dom-events/unlisten! (dom-xpath/xpath selector) :click)))

(defn destroy-todo-item [renderer [event path] transmitter]
  (let [view-div-id (render/get-id renderer path)
        view-div (dom/by-id view-div-id)
        ;; We have to remove the parent because id is attached to first
        ;; child of todo item
        parent (.-parentNode view-div)]
    (dom/destroy! parent)))

(defn create-count [r [_ path _]]
  (let [html (t/add-template r path (:count templates))]
    (dom/prepend! (dom/by-id "footer") (html {:number 0 :text "items"}))))

(defn create-clear-completed [r [_ path _]]
  (let [html (t/add-template r path (:clear-completed templates))]
    (dom/append! (dom/by-id "footer") (html {:number 0}))))

(defn destroy-clear-completed [_ _]
  (dom/destroy! (dom/by-id "clear-completed")))

(defn create-filter [r [_ path _]]
  (let [html (t/add-template r path (:filters templates))]
    (dom/prepend! (dom/by-id "footer") (html {:active "all"})))) ;; TODO: Use proper init value from behavior.

(defn update-count [r [event path _ new]]
  (let [key (last path)
        update-map (hash-map key (str new))]
    (t/update-parent-t r path update-map)))

(defn render-config []
  [[:node-create      [] render-simple-page]
   ;; TODO: Split create-todo-item into node-create and value fns
   [:transform-enable [:todo] enable-todo-transforms]
   [:transform-disable [:todo] disable-clear-completed-event]
   [:value            [:todo :*] create-todo-item]
   [:transform-enable [:todo :*] create-todo-item-event]
   [:node-destroy     [:todo :*] destroy-todo-item]
   [:node-create      [:count] create-count]
   [:value            [:count :*] update-count]
   [:node-create      [:filters] create-filter]
   [:transform-enable [:filters] enable-filter-transforms]
   [:node-create      [:clear-completed] create-clear-completed]
   [:node-destroy     [:clear-completed] destroy-clear-completed]
   [:value            [:clear-completed :*] update-count]])
