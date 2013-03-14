(ns marianoguerra.pipe-test
  (:use clojure.test
        marianoguerra.pipe))

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
    (is (continue? (continue 1)))
    (is (not (continue? (finish 1)))))

  (testing "finish?"
    (is (finish? (finish 1)))
    (is (not (finish? (continue 1)))))
  
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
                       #(continue % {:name :bob})))
           {:count 1 :name :bob}))
    (is (= (meta (pipe {:value 42}
                       #(continue % {:count 1})
                       #(continue % {:count 3 :age 27})
                       #(continue % {:name :bob})))
           {:count 3 :name :bob :age 27})))

  (testing "ops can be composed"
    (is (= (pipe {:value 42} (compose plus-one plus-one twice)) {:value 88}))
    (is (= (pipe {:value 42} (compose plus-one plus-one) twice) {:value 88})))

  (testing "or-pipe does the reverse of pipe"
    (is (= (or-pipe {:value 42} finish-value finish-value plus-one twice)
           {:value 43})))

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
