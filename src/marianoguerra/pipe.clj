(ns marianoguerra.pipe)

(defrecord Result [type data])

(defn finish? [{type :type}]
  (= type :finish))

(def continue? (complement finish?))

(defn- make-result [data type & [metadata]]
  (with-meta (->Result type data) metadata))

(defn- get-data [result]
  (if (instance? Result result)
    (:data result)
    result))

(defn continue [data & [metadata]]
  (make-result data :continue metadata))

(defn finish [data & [metadata]]
  (make-result data :finish metadata))

(defn- do-pipe [data stop? funs]
  (if (seq funs)
    (let [result ((first funs) data)
          new-meta (merge (meta data) (meta result))
          new-data (with-meta (get-data result) new-meta)]
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
