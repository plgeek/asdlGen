package asdl_base;
public final class String_option {
  private String_option() {}
  public String x;
  public String_option(String x) {
    this.x = x;
  }
  public String toString() { return ("SOME("+x+")");}
  public boolean equal(Object x) {
    if( x instanceof String_option) { 
      return ((String_option)x).x == this.x;
    } else {
      return false;
    }
  }
}



