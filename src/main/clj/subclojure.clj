
(ns subclojure
  (:require [clojure.tools.analyzer.jvm :as a.jvm]
            [clojure.tools.emitter.jvm :as e.jvm]
            [clojure.tools.emitter.jvm.emit :as e.jvm.emit]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.analyzer.passes.collect-closed-overs]))

(def file
  '((ns emit-test)

    (def x 12)

    (prn x)

    (defn foo []
      (def bar 17))

    (let [h 17]
      (def hh (fn [] h)))

    (defn bar [])))

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


(defn my-pass
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
                                   :var.static-fn/class-name (str (namespace var-sym) "$" (gensym (name var-sym)))
                                   :var.static-fn/method-name "invokeStatic"}
                                  {:var/kind :var.kind/constant
                                   :var.constant/field-name (str "var_" (name var-sym) "_value")})
                                {:var/kind :var.kind/changed})))))))
  ast)

(defn compile
  ([file] (compile file (clojure.lang.RT/makeClassLoader)))
  ([file cl]
   (binding [a.jvm/run-passes (clojure.tools.analyzer.passes/schedule (-> e.jvm/passes
                                                                          (conj #'my-pass)
                                                                          #_(disj #'clojure.tools.emitter.passes.jvm.collect/collect)))
             *vars* (atom {})]
     (let [analyze-opts {:bindings {Compiler/LOADER cl}}
           analyzed-forms (doall (map (fn [form]
                                        (with-bindings (:bindings analyze-opts)
                                          (a.jvm/analyze+eval form (a.jvm/empty-env) analyze-opts))) file))
           init-fn `(^:once fn* [] ~@(map :expanded-form analyzed-forms))

           classes (e.jvm.emit/emit-classes (a.jvm/analyze init-fn (a.jvm/empty-env) analyze-opts))
           r {:forms analyzed-forms
              :vars @*vars*
              :init-fn init-fn
              :bytecode classes}]
       (binding [*compile-files* true]
         (doseq [class classes]
           (e.jvm/compile-and-load class)))
       r))))

(def r (compile
        '((defn y ^long [^long x] (inc x)))))

(def cl (doto (clojure.lang.RT/makeClassLoader)
              (.addURL (.toURL (java.io.File. (System/getProperty "user.dir"))))))

(def loader-test
  (compile (list clojure.tools.emitter.temp/bootstrap-invoke) cl))

(defn print-ast [compile-res]
  (clojure.pprint/pprint
   (clojure.walk/postwalk
    (fn [form]
      (if (map? form)
        (dissoc form :env)
        form))
    (:forms compile-res))))

(def r (compile
        '((defn y ^long [^long x] (inc x)))))

(print-ast r)

