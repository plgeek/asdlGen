package asdl_base;

public class identifier {
  private identifier() {}
  private String s;
  public identifier(String s) {
    this.s = s.intern();
  }
  public String toString() { return s;}
  public int hashCode() { return s.hashCode(); }
  public boolean equal(Object x) {
    if( x instanceof identifier) { 
      return ((identifier)x).s == this.s;
    } else {
      return false;
    }
  }
}
