package asts.StdPrims;
import java.io.*;

final public class g extends asts.StdPkl.PklJava  {

  public static void write_java_lang_String(String x,OutputStream s) {
    int sz = x.length();
    int i = 0;
    try {
      asts.StdPkl.g.write_tag(sz,s); 
      while(i < sz) {
	s.write((byte)x.charAt(i++));
      }
    } catch (IOException e){
      die("Error writing String");
    }
  }

 public static String read_java_lang_String(InputStream s) {
    int sz  = asts.StdPkl.g.read_tag(s);
    StringBuffer sb = new StringBuffer(sz);
    try {
      while(sz > 0) {
	sb.append((char)s.read());
	sz--;
      }
    } catch(IOException e) {
      die("Error reading int");
    }
    return sb.toString();
  }
 
  public static identifier read_identifier(InputStream s) {
    return new identifier(read_java_lang_String(s));
  }

  public static void write_identifier(identifier x,OutputStream s) {
    write_java_lang_String(x.toString(),s);
  }

}

