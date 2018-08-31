package asdl_base;
public final class identifier_option {
  private identifier_option() {}
  public identifier x;
  public identifier_option(identifier x) {
    this.x = x;
  }
  public String toString() { return ("SOME("+x+")");}
  public boolean equal(Object x) {
    if( x instanceof identifier_option) { 
      return ((identifier_option)x).x == this.x;
    } else {
      return false;
    }
  }
}


