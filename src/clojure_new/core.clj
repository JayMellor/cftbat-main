(ns clojure-new.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot!"))   

(println "Cleanliness is the next godliness")

(defn train
  []
  (println "a train"))


(def res (+ 1 (* 2 3) 4))

(if true
  (do (println "first branch") "second thing")
  (do (println "second branch") "first thing")
)

(when true
  (println "printed")
  "here's a message")

(nil? false)

(= false 0)

(and 1 2 nil)

(def nom "elephant")
(str  "something" nom)


(({:something 2 :other_thing 3 :nessted {:inner_one 1}} 
  :nessted) :inner_one)

(defn addd
  "adds numbers"
  ([]  nil)
  ([first] first)
  ([first second] (+ first second))
  ([first second third] (+ first (+ second third))))

(defn better-add
  "adds numbers using variadicity"
  [& args]
  (reduce + args))

(defn firrst
  [[fst]]
  fst)

(defn swap-one-and-two
  [{one :one two :two}]
  {:one two :two one})

(defn swap-quicker
  [{:keys [one two]}]
  {:one two :two one})

(defn multi-form
  [thing]
  (+ 1 2 3)
  (str thing "<- from you"))

#(* % 3)
; cf. (* 2 3)
; can also use %1, %2, and %&


(def asym-hobbit-body-parts [
  {:name "head" :size 3}
  {:name "left-eye" :size 1}
  {:name "left-ear" :size 1}
  {:name "mouth"  :size 1}
  {:name "neck"  :size 2}
  {:name "left-shoulder" :size 3}
  {:name "left-upper-arm" :size 3}
  {:name "chest" :size 10}
  {:name "back" :size 10}
  {:name "left-forearm" :size 3}
  {:name "abdomen" :size 6}
  {:name "left-kidney" :size 1}
  {:name "left-hand" :size 2}
  {:name "left-knee" :size 2}
  {:name "left-thigh" :size 4}
  {:name "left-lower-leg" :size 3}
  {:name "left-achilles" :size 1}
  {:name "left-foot" :size 2}
  ])

(defn matching-part [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
  :size (:size part)})

(let [result (inc 4)] result)

(into [] #{:a :b})

(loop [idx 3]
  (println idx) ; Prints 3 -> 1
  (if (> idx 1) 
    (recur (dec idx)) ; next 'iteration'
    (str idx)))

(defn symmetrize-body-parts
  [asym-body-parts]
  (loop [remaining-asyms asym-body-parts ; remaining-asyms <- asym-body-parts
         finals []] ; finals <- []
    (if (empty? remaining-asyms)
      finals
      (let [[part & remaining] remaining-asyms]
        (recur remaining ; remaining-asyms <- remaining
               (into finals ; finals <- finals and part with matching part
                     (set [part (matching-part part)])))))))

(defn symmetrize [acc cur]
  (into acc (set [cur (matching-part cur)])))

(defn my-symmetrize
  [asym-collection]
  (reduce symmetrize [] asym-collection))

(defn gen-part
  [{:keys [name size]}]
  (fn [idx]
   {:name (clojure.string/replace name #"^left-" (str "right" idx "-"))
    :size size}))

(defn gen-parts
  [parts-to-add part]
  (into [part] 
    (map (gen-part part) (range parts-to-add))))

(defn add-parts 
  [parts-to-add]
  (fn [acc cur] ; can also use partial
   (into acc (set (gen-parts parts-to-add cur)))))

(defn alienate
  [parts-to-add asym-collection]
  (reduce (add-parts parts-to-add) [] asym-collection))

(defn dec-maker
  [dec-by]
  #(- % dec-by)
  (fn [num] (- num dec-by))
)

(defn map-set
  [map-fn vec]
  (set (map map-fn vec)))

(defn map-from-reduce
  [func coll]
  (reduce #(conj %1 (func %2)) []  coll))

(defn filter-from-reduce
  [func coll]
  (reduce #(if (func %2)
               (conj %1 %2)
               %1) [] coll))

(def conj-ex
  (apply conj [1 2] [:elephant :something]))

(defn partial-from-apply
  [func & args]
  (fn 
    [& other-args] 
    (apply func (into args other-args))))

(defn my-complement
  [func]
  (fn [& args] (not (apply func args))))

(defn reduce-from-loop
  ([func coll]
   ;; fix initial val default
   (reduce-from-loop func 0 coll))
  ([func init-val  coll]
   (loop [acc init-val
          curr (first coll)
          rem-coll (rest coll)]
     (println acc curr rem-coll)
     (if (empty? rem-coll)
       (func acc curr)
       (recur (func acc curr) (first rem-coll) (rest rem-coll) )))))

;;;;;;;;;
;; Macros
;;;;;;;;;

(defmacro backwards
  [form]
  (reverse form))

(backwards (1 2 3 +)) ;=> 6

(eval 
 (let [infix (read-string "(1 + 1)")] 
   (list (second infix) (first infix) (last infix))))



; Macros execute between reading and evaluating

(defmacro ignore-last
  [func-call]
  (butlast func-call))

(defmacro pprintt
  [inp]
  (println inp)
  inp)

(defn eval-arithmetic
  [form-string]
  (let [form-list (read-string form-string)]
    (eval (infix-to-rpn form-list))
    ))

(defn rearrange-operator
  [sym form-seq]
  (->> form-seq
       (partition-by #(= % sym))
       (map #(if (and (seq? %) (= 1 (count %))) (first %) %))
       (filter #(not= % sym))
       (cons sym)))

(defn infix-to-rpn
  [form-seq]
  (->> form-seq
       (rearrange-operator '+)
       (map #(if (seq? %) (rearrange-operator '* %) %))))

(defmacro unless
  [test & branches]
  (conj  (reverse branches) test 'if))

;; Syntax Quoting

; `sym reveals symbol in namespace
`(+ 9 ~(inc 2)) ;=> (clojure.core/+ 9 3)

(defmacro code-critic-base
  "Phrases are courtesy of Hermes Conrad from Futurama"
  [bad good]
  (list 'do
        (list 'println
              "Great Squid of Madrid, this is bad code:"
              (list 'quote bad))
        (list 'println
              "Sweet gorilla of Manila, this is good code:"
              (list 'quote good))))

(defmacro code-critic
  [bad-form good-form]
  `(do (println "this is baad:" 
                '~bad-form)
       (println "Ooh this is good:"
                '~good-form)))

(defmacro with-local
  [& args]
  (let [macro-message (gensym 'message)]
    `(let [~macro-message "this is locally scoped"]
       ~@args
       (println ~macro-message))))

; clojure-new.core> (macroexpand '(with-local (println "fred")))
; =>
(let*
    [message11084 "this is locally scoped"]
    (println "fred")
    (clojure.core/println message11084))

(defmacro with-local-concise
  "Same macro with auto-gensym"
  [& args]
  `(let [macro-message# "local message"]
     ~@args
     (println macro-message#)))

(macroexpand '(with-local-concise (println "fred")))
; =>
(let*
 [macro-message__11912__auto__ "local message"]
 (println "fred")
 (clojure.core/println macro-message__11912__auto__))


; ----

(defn order-valid?
  [details validations]
;  (println details validations)
  (true? (and details validations)))

(defmacro when-valid
  [order-details order-details-validations & then]
  `(when (order-valid? ~order-details ~order-details-validations) 
    ~@then))

(defmacro my-or
  ([]
   nil)
  ([form]
   form)
  ([form & forms]
   `(loop [first# ~form
           rest# '~forms]
      (if first#
        first#
        (recur (first rest#) (rest rest#))))))
; above can be done with recursion (probably better than this)

; ----

(def character
  {:name "A Person"
   :attributes {:intelligence 10
                :strength 5
                :dexterity 4}})

; Create macro that generates functions as below
(def c-int (comp :intelligence :attributes))

(defmacro defattrs
  ([] '())
  ([name attr  & rest]
   (when (even? (count rest))
     `((def ~name (comp ~attr :attributes))
       (defattrs ~@rest)))))

; ----

(do (future (Thread/sleep 1000)
          (println "finally!"))
 (println "immediate"))

(let [f (future)]
  @f
  (println (realized? f)))

(def message-for-later
  (delay (let [msg "from the future!"]
           ; Only printed the first time; result
           ; is cached thereafter
           (println "First time: " msg)
           (Thread/sleep 1000)
           msg)))
@message-for-later


(defn get-resource
  [url]
  (Thread/sleep 1000)
  (first url))

(defn action-printer
  []
  (println "Started")
  (let [resource (future (get-resource "url.address.com"))]
    (println "Loading")
    (println "Response" @resource)))

(def my-promise (promise))
(deliver my-promise (+ 1 2))
@my-promise

(time  (let [res-promise (promise)]
         (doseq [url ["first" "second" "third"]]
           (future (if-let [final-res (= \t (get-resource url))]
                     (deliver res-promise final-res))))
         (println "final res:" (deref res-promise 3000 "timed out"))))
