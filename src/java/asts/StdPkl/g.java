package asts.StdPkl;
final public class g extends PklJava {
    public static void write_tag (int x, java.io.OutputStream s) {
	PklJava.write_java_int(x,s);
    }
    public static int read_tag (java.io.InputStream s) {
	return PklJava.read_java_int(s);
    }
}
