(ns marianoguerra.pipe)

(defn finish? [value]
  (true? (::finish (meta value))))

(defn error? [value]
  (true? (::error (meta value))))

(def ok? (complement error?))
(def continue? (complement finish?))

(defn finish [data & [metadata]]
  (with-meta data (merge (meta data) metadata {::finish true})))

(defn continue [data & [metadata]]
  (with-meta data (merge (meta data) metadata)))

(defn error [data & [metadata]]
  (finish data (merge metadata {::error true})))

(defn make-error [reason type]
  (error {:reason reason :type type}))

(defn- clear-meta-key [value key]
  (with-meta value (dissoc (meta value) key)))

(defn clear-error [value]
  (clear-meta-key value ::error))

(defn- clear-pipe-meta [value]
  (clear-meta-key value ::finish))

(defn- do-pipe [data stop? funs & keep-meta]
  (if (seq funs)
    (let [result ((first funs) data)
          new-meta (merge (meta data) (meta result))
          new-data (with-meta result new-meta)]
      (if (stop? result)
        (if keep-meta
          new-data
          (clear-pipe-meta new-data))
        (recur new-data stop? (rest funs) keep-meta)))

    data))

(defn pipe [data & funs]
  (do-pipe data finish? funs))

(defn or-pipe [data & funs]
  (do-pipe data continue? funs))

(defn compose [& funs]
  (fn [data]
    (do-pipe data finish? funs true)))

(defn or-compose [& funs]
  (fn [data]
    (do-pipe data continue? funs)))
