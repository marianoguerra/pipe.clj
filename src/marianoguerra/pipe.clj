(ns marianoguerra.pipe)

(defrecord Result [type data])

(defn finish? [arg]
  (= (:type arg) :finish))

(def continue? (complement finish?))

(defn- make-result [data type & [metadata]]
  (let [new-data (if metadata
                   (with-meta data metadata)
                   data)
        result (->Result type new-data)]
    result))

(defn get-data [result]
  (if (instance? Result result)
    (:data result)
    result))

(defn continue [data & [metadata]]
  (make-result data :continue metadata))

(defn finish [data & [metadata]]
  (make-result data :finish metadata))

(defn error [data & [metadata]]
  (finish data (merge metadata {:error true})))

(defn make-error [reason type]
  (error {:reason reason :type type}))

(defn error? [value]
  (:error (meta (get-data value))))

(defn- do-pipe [data stop? funs]
  (if (seq funs)
    (let [result ((first funs) data)
          new-meta (merge (meta data) (meta (get-data result)))
          result-data (get-data result)
          new-data (if new-meta (with-meta result-data new-meta) result-data)]
      (if (stop? result)
        new-data
        (recur new-data stop? (rest funs))))

    data))

(defn pipe [data & funs]
  (do-pipe data finish? funs))

(defn or-pipe [data & funs]
  (do-pipe data continue? funs))

(defn compose [& funs]
  (fn [data]
    (apply pipe data funs)))

(defn or-compose [& funs]
  (fn [data]
    (apply or-pipe data funs)))
