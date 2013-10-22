(ns marianoguerra.pipe)

(defn finish? [value]
  (true? (::finish (meta value))))

(defn error? [value]
  (true? (::error (meta value))))

(def ok? (complement error?))
(def continue? (complement finish?))

(defn finish [data & [metadata]]
  (vary-meta data merge (assoc metadata ::finish true)))

(defn continue [data & [metadata]]
  (vary-meta data merge metadata))

(defn error [data & [metadata]]
  (finish data (merge metadata {::error true})))

(defn make-error [reason type]
  (error {:reason reason :type type}))

(defn- clear-meta-key [value key]
  (vary-meta value dissoc key))

(defn clear-error [value]
  (clear-meta-key value ::error))

(defn- clear-pipe-meta [value]
  (clear-meta-key value ::finish))

(defn- do-pipe [data stop? funs & keep-meta]
  (if (seq funs)
    (let [result ((first funs) data)
          new-data (if (nil? result)
                     result
                     (with-meta result (merge (meta data) (meta result))))]
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
