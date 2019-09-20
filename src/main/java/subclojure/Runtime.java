package subclojure;

import clojure.lang.IFn;
import clojure.lang.Namespace;
import clojure.lang.Symbol;
import clojure.lang.Var;

import java.lang.invoke.*;

public class Runtime {
    public static final Object UNBOUND = new Object();
    public static final Object UNINITIALIZED_FN = new Object();

    public static class MetaVar {
        public volatile Var var;
        public volatile SwitchPoint switch_point;
        public MethodHandle deref_mh;
        public MethodHandle invoke_mh;
        public volatile MethodHandle deref_fallback_mh;
        public volatile MethodHandle invoke_fallback_mh;
        public volatile boolean invalid;
        public volatile Object value;
    }

    public static CallSite bootstrapInvoke(MethodHandles.Lookup caller, MethodType methodType, MetaVar meta, Namespace ns, String varName, Class fnClass) {
        try {
            if (meta.var == null) {
                meta.var = new Var(ns, Symbol.intern(varName));
            }
            if (meta.invoke_fallback_mh == null) {
                MethodType boxedMethodType = methodType.changeReturnType(Object.class);
                for (int i = 0; i < methodType.parameterCount(); i++) {
                    boxedMethodType = boxedMethodType.changeParameterType(i, Object.class);
                }
                meta.invoke_fallback_mh = caller.findVirtual(Var.class, "invoke", boxedMethodType).bindTo(meta.var).asType(methodType);
            }
            if (meta.invalid) {
                return new ConstantCallSite(meta.invoke_fallback_mh);
            } else if (meta.value == UNBOUND) {
                return new ConstantCallSite(meta.invoke_fallback_mh);
            } else {
                if (meta.invoke_mh != null) {
                    return new ConstantCallSite(meta.invoke_mh);
                } else {
                    if (meta.switch_point == null) {
                        meta.switch_point = new SwitchPoint();
                    }

                    if (meta.value == UNINITIALIZED_FN) {
                        meta.invoke_mh = meta.switch_point.guardWithTest(
                                caller.findStatic(fnClass, "invokeStatic", methodType),
                                meta.invoke_fallback_mh);
                        return new ConstantCallSite(meta.invoke_mh);
                    } else {
                        meta.invoke_mh = meta.switch_point.guardWithTest(
                                caller.findVirtual(IFn.class, "invoke", methodType).bindTo(meta.value),
                                meta.invoke_fallback_mh);
                        return new ConstantCallSite(meta.invoke_mh);
                    }
                }
            }
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    public static CallSite bootstrapDeref(MethodHandles.Lookup caller, MetaVar meta, Namespace ns, String varName, Class fnClass) {
        try {
            if (meta.var == null) {
                meta.var = new Var(ns, Symbol.intern(varName));
            }
            if (meta.deref_fallback_mh == null) {
                meta.deref_fallback_mh = caller.findVirtual(Var.class, "deref", MethodType.methodType(Object.class)).bindTo(meta.var);
            }
            if (meta.invalid) {
                return new ConstantCallSite(meta.deref_fallback_mh);
            } else if (meta.value == UNBOUND) {
                return new ConstantCallSite(meta.deref_fallback_mh);
            } else {
                if (meta.deref_mh != null) {
                    return new ConstantCallSite(meta.deref_mh);
                } else {
                    if (meta.switch_point == null) {
                        meta.switch_point = new SwitchPoint();
                    }
                    if (meta.value == UNINITIALIZED_FN) {
                        meta.value = fnClass.newInstance();
                    }
                    meta.deref_mh = meta.switch_point.guardWithTest(MethodHandles.constant(Object.class, meta.value), meta.deref_fallback_mh);
                    return new ConstantCallSite(meta.deref_mh);
                }
            }
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
}
