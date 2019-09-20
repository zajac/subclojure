
(ns subclojure
  (:require [clojure.tools.analyzer.jvm :as a.jvm]
            [clojure.tools.emitter.jvm :as e.jvm]
            [clojure.tools.emitter.jvm.emit :as e.jvm.emit]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.analyzer.passes.collect-closed-overs]))



(def ^:dynamic *vars*)
(defn collect-vars-pass-state []
  *vars*)

(defn var-sym [var]
  (let [{:keys [name ns]} (meta var)]
    (symbol (clojure.core/name (ns-name ns)) (clojure.core/name name))))

{:var/kind #{:var.kind/constant :var.kind/static-fn :var.kind/changed :var.kind/declared}
 :var.static-fn/class-name "fn$1212"
 :var.static-fn/method-name "invokeStatic"
 :var.constant/field-name "var_x_value"}

(defn collect-vars-info
  {:pass-info {:walk :pre
               :depends #{#'clojure.tools.analyzer.passes.collect-closed-overs/collect-closed-overs}
               :state collect-vars-pass-state}}
  [state ast]
  (when (= (:op ast) :def)
    (swap! state (fn [state]
                   (let [{:keys [var init]} ast
                         var-sym (var-sym var)
                         declared? (:declared (meta var))
                         static-fn? (and (= (:op init) :with-meta)
                                         (= (:op (:expr init)) :fn)
                                         (empty? (:closed-overs (:expr init))))
                         var-info (get state var-sym)]
                     (when (= var-sym `hh)
                       (def shit ast))
                     (assoc state var-sym
                            (if (and declared? (nil? var-info))
                              {:var/kind :var.kind/declared}
                              (if (or (= (:var/kind var-info) :var.kind/declared)
                                      (nil? var-info))
                                (if static-fn?
                                  {:var/kind :var.kind/static-fn
                                   :var.static-fn/class-name (str (namespace var-sym) "$" (name var-sym))
                                   :var.static-fn/method-name "invokeStatic"}
                                  {:var/kind :var.kind/constant
                                   :var.constant/field-name (str "var_" (name var-sym) "_value")})
                                {:var/kind :var.kind/changed})))))))
  ast)

(defn provide-invoke-static
  {:pass-info {:walk :pre
               :depends #{#'clojure.tools.analyzer.passes.collect-closed-overs/collect-closed-overs}}}
  [ast]
  (if (and (= (:op ast) :fn) (empty? (:closed-overs ast)))
    (update ast :methods (fn [methods] (into [] (map #(assoc % :static? true)) methods)))
    ast))

(defn generate-bootstrap-invoke [loader-class var-infos]
  (let [method-name-sym (symbol "method_name")
        caller-sym (symbol "caller")
        method-type-sym (symbol "method-type")
        var-name-sym (symbol "var-name")
        namespace-field (symbol loader-class "namespace")]
    `(fn [~caller-sym ~method-name-sym ~method-type-sym ~var-name-sym]
      (cond
        ~@(->> var-infos
           (filter (fn [[var-sym info]] (= :var.kind/static-fn (:var/kind info))))
           (mapcat
            (fn [[var-sym var-info]]
              (let [meta-field (symbol loader-class (name var-sym))]
                `[(identical? ~var-name-sym ~(name var-sym))
                  (do
                    (when (nil? ~meta-field)
                      (let [meta# (subclojure.Runtime$MetaVar.)]
                        (set! (.-value meta#) subclojure.Runtime/UNINITIALIZED_FN)
                        (set! ~meta-field meta#)))
                    (subclojure.Runtime/bootstrapInvoke ~caller-sym
                                                        ~method-type-sym
                                                        ~meta-field
                                                        ~namespace-field
                                                        ~(name var-sym)
                                                        (Class/forName ~(:var.static-fn/class-name var-info))))]))))
        :else (throw (ex-info "not supported" {:var-name ~var-name-sym}))))))

(defn compile
  ([file] (compile file (clojure.lang.RT/makeClassLoader)))
  ([file cl]
   (binding [a.jvm/run-passes (clojure.tools.analyzer.passes/schedule (conj e.jvm/passes
                                                                            #'collect-vars-info
                                                                            #'provide-invoke-static))
             *vars* (atom {})
             clojure.tools.analyzer.passes.elide-meta/elides {:all #{:doc :file :line :added :column}}]
     (let [analyze-opts {:bindings {Compiler/LOADER cl}}
           analyzed-forms (doall (map (fn [form]
                                        (with-bindings (:bindings analyze-opts)
                                          (a.jvm/analyze+eval form (a.jvm/empty-env) analyze-opts))) file))
           var-infos @*vars*
           init-fn `(^:once fn* [] ~@(map :expanded-form analyzed-forms))
           init-ast (a.jvm/analyze init-fn (a.jvm/empty-env) analyze-opts)
           root-ns (get-in init-ast [:env :ns])
           classes (binding [e.jvm.emit/*vars-info* var-infos]
                     (e.jvm.emit/emit-classes (assoc init-ast :class-name (e.jvm.emit/loader-class root-ns))))
           meta-var-fields (->> var-infos
                                (map (fn [[var-sym var-info]]
                                       {:op :field,
                                        :attr #{:public :static},
                                        :name (name var-sym),
                                        :tag :subclojure.Runtime$MetaVar})))
           loader-bc (-> (last classes)
                         (update :fields concat meta-var-fields)
                         (update :fields conj {:op :field
                                               :attr #{:public :static}
                                               :name "namespace"
                                               :tag :clojure.lang.Namespace}))
           classes (concat (butlast classes) [loader-bc])
           bootstrap-form (generate-bootstrap-invoke (:name loader-bc) var-infos)
           _ (binding [*compile-files* true]
               (doseq [class classes]
                 (e.jvm/compile-and-load class)))
           bootstrap-bc (-> (a.jvm/analyze bootstrap-form (a.jvm/empty-env) analyze-opts)
                            (assoc :class-name (e.jvm.emit/bootstrap-invoke-class root-ns))
                            (e.jvm.emit/emit-classes)
                            (first)
                            (assoc :super java.lang.Object)
                            (update :methods #(keep (fn [method]
                                                      (cond
                                                        (= (:method method) [[:<clinit>] :void]) method
                                                        (= (:method method) [[:invokeStatic Object Object Object Object] Object])
                                                        (assoc method :method [[:bootstrapInvoke
                                                                                java.lang.invoke.MethodHandles$Lookup
                                                                                java.lang.String
                                                                                java.lang.invoke.MethodType
                                                                                String]
                                                                               java.lang.invoke.CallSite])))
                                               %)))]
       (binding [*compile-files* true]
         (e.jvm/compile-and-load bootstrap-bc))
       {:forms analyzed-forms
        :vars var-infos
        :bytecode classes
        :loader-bc loader-bc
        :bootstrap bootstrap-bc}))))

(defn print-ast [compile-res]
  (clojure.pprint/pprint
   (clojure.walk/postwalk
    (fn [form]
      (if (map? form)
        (dissoc form :env)
        form))
    (:forms compile-res))))


(comment
  (def file
  '((ns emit-test)

    (def x 12)

    (prn x)

    (defn foo []
      (def bar 17))

    (let [h 17]
      (def hh (fn [] h)))

    (defn bar [])))


  (def bootstrap-sample (emit-bootstrap-invoke "subclojure__init" "y" "subclojure$y"))

  (def bootstrap-sample (emit-bootstrap-invoke "subclojure.SampleNS" "x" "subclojure.MyLambda"))


(def cl (doto (clojure.lang.RT/makeClassLoader)
              (.addURL (.toURL (java.io.File. (System/getProperty "user.dir") "src/main/java")))
              (.addURL (.toURL (java.io.File. (System/getProperty "user.dir") "classes")))))

  (def r (compile [bootstrap-sample] cl))
*e
(require 'nsloader)

(def loader-test
  (compile (list nsloader/bootstrap-invoke) cl))
(pprint init-ast)



(def r (compile
        '((ns fuck)
          (defn y ^long [^long x] (inc x))
          (y 1))))


  (pprint (:bootstrap r))

  (pprint (:loader-bc r))

  (pprint (:forms r))

*e
  (pprint (:bytecode r))

(.invoke (.newInstance (Class/forName "subclojure__init" true cl)))

  (.invoke (.newInstance (Class/forName "fuck2__init" true cl)))

  subclojure__init

  ((.newInstance (Class/forName (:class-name (second (:bytecode r))) true cl)))

(print-ast r)



(def test-expr `((ns ~(symbol "fuck2"))
                 ~@(map (fn [i] `(defn ~(symbol (str "fun" i)) [x# y# z#] y#)) (range 200))
                 ~@(map (fn [i] `(~(symbol (str "fun" i)) 1 2 3)) (range 200))
                 ))

  (def t
    (compile test-expr))

(.invoke (.newInstance (Class/forName "fuck2__init" true cl)))

  )
*e