package subclojure;

import clojure.lang.IFn;
import clojure.lang.Namespace;
import clojure.lang.Symbol;
import clojure.lang.Var;

import java.lang.invoke.*;

public class SampleNS {
    public static final Object UNBOUND = new Object();
    public static final Object UNINITIALIZED_FN = new Object();

    public static Namespace ns;

    public static volatile Var var_x_var;
    public static volatile SwitchPoint var_x_switchPoint;
    public static MethodHandle var_x_deref_mh;
    public static MethodHandle var_x_invoke_mh;
    public static volatile MethodHandle var_x_deref_fallback_mh;
    public static volatile MethodHandle var_x_invoke_fallback_mh;
    public static volatile boolean var_x_invalid;
    public static volatile Object var_x_value;


    /**
     * @noinspection StringEquality
     */
    public static CallSite bootstrapDeref(MethodHandles.Lookup caller, String methodName, MethodType methodType, String varName) {
        if (varName == "x") {
            try {
                if (var_x_var == null) {
                    var_x_var = new Var(ns, Symbol.intern("x"));
                }
                if (var_x_deref_fallback_mh == null) {
                    var_x_deref_fallback_mh = caller.findVirtual(Var.class, "deref", MethodType.methodType(Object.class)).bindTo(var_x_var);
                }
                if (var_x_invalid) {
                    return new ConstantCallSite(var_x_deref_fallback_mh);
                } else if (var_x_value == UNBOUND) {
                    return new ConstantCallSite(var_x_deref_fallback_mh);
                } else {
                    if (var_x_deref_mh != null) {
                        return new ConstantCallSite(var_x_deref_mh);
                    } else {
                        if (var_x_switchPoint == null) {
                            var_x_switchPoint = new SwitchPoint();
                        }
                        if (var_x_value == UNINITIALIZED_FN) {
                            var_x_value = new MyLambda();
                        }
                        var_x_deref_mh = var_x_switchPoint.guardWithTest(MethodHandles.constant(Object.class, var_x_value), var_x_deref_fallback_mh);
                        return new ConstantCallSite(var_x_deref_mh);
                    }
                }
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }
        } else {
            throw new IllegalArgumentException();
        }
    }


    /**
     * @noinspection StringEquality
     */
    public static CallSite bootstrapInvoke(MethodHandles.Lookup caller, String methodName, MethodType methodType, String varName) {
        if (varName == "x") {
            try {
                if (var_x_var == null) {
                    var_x_var = new Var(ns, Symbol.intern("x"));
                }
                if (var_x_invoke_fallback_mh == null) {
                    var_x_invoke_fallback_mh = caller.findVirtual(Var.class, "invoke", methodType).bindTo(var_x_var);
                }
                if (var_x_invalid) {
                    return new ConstantCallSite(var_x_invoke_fallback_mh);
                } else if (var_x_value == UNBOUND) {
                    return new ConstantCallSite(var_x_invoke_fallback_mh);
                } else {
                    if (var_x_invoke_mh != null) {
                        return new ConstantCallSite(var_x_invoke_mh);
                    } else {
                        if (var_x_switchPoint == null) {
                            var_x_switchPoint = new SwitchPoint();
                        }

                        if (var_x_value == UNINITIALIZED_FN) {
                            var_x_invoke_mh = var_x_switchPoint.guardWithTest(
                                    caller.findStatic(MyLambda.class, "invokeStatic", methodType),
                                    var_x_invoke_fallback_mh);
                            return new ConstantCallSite(var_x_invoke_mh);
                        } else {
                            var_x_invoke_mh = var_x_switchPoint.guardWithTest(
                                    caller.findVirtual(IFn.class, "invoke", methodType).bindTo(var_x_value),
                                    var_x_invoke_fallback_mh);
                            return new ConstantCallSite(var_x_invoke_mh);
                        }
                    }
                }
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }
        } else {
            throw new IllegalArgumentException();
        }
    }
}
