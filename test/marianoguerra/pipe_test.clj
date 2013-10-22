(ns marianoguerra.pipe-test
  (:use clojure.test
        marianoguerra.pipe))

(defn return-nil [_]
  nil)

(defn plus-one [{value :value}]
  {:value (+ value 1)})

(defn twice [{value :value}]
  {:value (* value 2)})

(defn finish-value [value]
  (finish value))

(defn pass [value]
  value)

(defn is-admin [{:keys [username] :as value}]
  (if (= username "admin")
    (continue value)
    (finish {:reason :not-admin :value value} {:error true})))

(defn has-role [role]
  (fn [{:keys [roles] :as value}]
    (if (contains? (set roles) role)
      (continue value)
      (finish {:reason :no-valid-role :expected role :value value} {:error true}))))

(defn is-over-age [min-age]
  (fn [{:keys [age] :as value}]
    (if (>= age min-age)
      (continue value)
      (finish {:reason :no-valid-age :expected min-age :value value} {:error true}))))

(def has-admin-permission (or-compose (has-role :admin) is-admin))

(deftest pipe-test
  (testing "continue?"
    (is (continue? (continue {:value 1})))
    (is (not (continue? (finish {:value 1})))))

  (testing "finish?"
    (is (finish? (finish {:value 1})))
    (is (not (finish? (continue {:value 1})))))

  (testing "error?"
    (is (error? (error {})))
    (is (not (error? (finish {})))))
  
  (testing "piping data to empty pipe returns value"
    (is (= (pipe {:value 42}) {:value 42})))

  (testing "piping more than one function returns value"
    (is (= (pipe {:value 42} plus-one) {:value 43}))
    (is (= (pipe {:value 42} plus-one twice) {:value 86})))

  (testing "returns prematurely if one op returns a finish result"
    (is (= (pipe {:value 42} plus-one finish-value twice) {:value 43}))
    (is (= (pipe {:value 42} plus-one twice finish-value) {:value 86}))
    (is (= (pipe {:value 42} plus-one twice finish-value twice) {:value 86})))

  (testing "metadata is kept and merged"
    (is (= (meta (pipe {:value 42} #(continue % {:count 1}))) {:count 1}))
    (is (= (meta (pipe {:value 42} #(continue % {:count 1}) pass)) {:count 1}))
    (is (= (meta (pipe {:value 42}
                       #(continue % {:count 1})
                       pass
                       #(continue % {:name :bob})
                       #(error % {:type :error-type})
                       #(finish % {:this-should-not-go 42})))
           {:count 1 :name :bob :marianoguerra.pipe/error true :type :error-type}))
    (is (= (meta (pipe {:value 42}
                       #(continue % {:count 1})
                       #(continue % {:count 3 :age 27})
                       #(continue % {:name :bob})))
           {:count 3 :name :bob :age 27})))

  (testing "ops can be composed"
    (is (= (pipe {:value 42} (compose plus-one plus-one twice)) {:value 88}))
    (is (= (pipe {:value 42} (compose plus-one plus-one) twice) {:value 88})))

  (testing "composed ops short circuit on error"
    (let [fail-last (compose plus-one
                             #(error % {:type :fail}))
          fail-first (compose #(error % {:type :fail})
                              plus-one)
          value {:value 42}
          result-fail-last (pipe value fail-last)
          result-fail-first (pipe value fail-first)]
    (is (error? result-fail-last))
    (is (error? result-fail-first))
    (is (= result-fail-last {:value 43}))
    (is (= result-fail-first {:value 42}))))

  (testing "compose inside a pipe keeps metadata for parent pipe"
    (let [fail-first (compose #(error % {:type :fail})
                              plus-one)
          value {:value 42}
          result (pipe value fail-first plus-one)]
      (is (error? result))
      (is (= result {:value 42}))))


  (testing "or-pipe does the reverse of pipe"
    (is (= (or-pipe {:value 42} finish-value finish-value plus-one twice)
           {:value 43})))

  (testing "function returning nil doesn't fail"
    (is (nil? (pipe {:value 42} return-nil))))

  (testing "complex example"
    (let [user {:username "bob" :age 10 :roles #{:user}}
          admin {:username "admin" :age 27 :roles #{:user :role-manager}}
          super-user {:username "alice" :age 28 :roles #{:user :admin}}]

      (is (= (:reason (pipe user (is-over-age 11) has-admin-permission))
             :no-valid-age))

      (is (= (:reason (pipe user (is-over-age 10) has-admin-permission))
             :not-admin))

      (is (= (pipe admin (is-over-age 10) has-admin-permission))
             admin)

      (is (= (pipe super-user (is-over-age 10) has-admin-permission))
             super-user)))

  (testing "or-compose does the opositve of compose"
    (is (= (pipe {:value 42} (or-compose finish-value finish-value plus-one twice))
           {:value 43}))
    (is (= (pipe {:value 42} (or-compose finish-value finish-value plus-one) twice)
           {:value 86}))))
