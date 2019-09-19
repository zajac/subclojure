package subclojure;

public class MyLambda {
    public Object invoke() {
        System.out.println("hi");
        return "hi";
    }

    public static Object invokeStatic() {
        System.out.println("hi static");
        return "hi static";
    }
}
