package asdl_base;
public final class int_option {
  private int_option() {}
  public int x;
  public int_option(int x) {
    this.x = x;
  }
  public String toString() { return ("SOME("+x+")");}
  public boolean equal(Object x) {
    if( x instanceof int_option) { 
      return ((int_option)x).x == this.x;
    } else {
      return false;
    }
  }
}



