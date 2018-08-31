package asdl_base;
import java.io.*;
import java.math.BigInteger;

public  class Prims {

  private static int set_neg(int x) { return (x | 0x40); }
  private static int set_neg(long x) { return (int)(x | 0x40); }

  private static int mask_neg(int x) { return (x & 0x3f); }

  private static int set_continue(int x) { return (x | 0x80); }
  private static int set_continue(long x) { return (int)(x | 0x80); }
 
  private static 
    boolean is_continue_bit_set(int x) { return ((x & 0x80) != 0); }
  
  private static
    boolean is_neg_bit_set(int x) { return ((x & 0x40) != 0); }
  
  private static int nibble(int x) { return (x & 0x07f); }
  private static int nibble(long x) { return (int)(x & 0x07f); }

  private static void die(String s) {
      System.err.println(s);
      System.exit(-1);
  }
  
  /* boolean */
  public static void write_java_boolean(boolean x,OutputStream s) {
    /* bools are tags that begin at 1 */
    if(x) {
      write_java_int(2,s); 
    } else {
      write_java_int(1,s);
    }
  }

  public static boolean read_java_boolean(InputStream s) {
       return (read_java_int(s) != 1);   
  }
  /* byte */
  public static void write_java_byte(byte x,OutputStream s) {
    write_java_int(x,s);
  }

  public static byte read_java_byte(InputStream s) {
       return (byte)read_java_int(s);   
  }

  /* short */
  public static void write_java_short(short x,OutputStream s) {
    write_java_int(x,s);
  }

  public static short read_java_short(InputStream s) {
       return (short)read_java_int(s);   
  }

  /* int */
  public static void write_java_int(int x,OutputStream s) {
    try {
      boolean  is_neg  =  (x < 0) ;      
     
      if (x == Integer.MIN_VALUE) {
	/* handle 2's complement asymmetry */
	s.write(set_continue(Math.abs(Integer.MIN_VALUE % 128)));
	x = Math.abs(Integer.MIN_VALUE / 128); 
      } else {
	x = Math.abs(x);
      }

      while( x > 63) {
	s.write(set_continue(nibble(x)));
	x >>= 7;
      }

      if(is_neg) { s.write(set_neg(x)); }
      else { s.write(x); }

    } catch (IOException e) {
      die("Error writing int");
    }
  }


  public static int read_java_int(InputStream s) {
    int acc = 0;
    int shift = 0;
    int x;

     try {
       x = s.read();
       while(is_continue_bit_set(x)) {
	 acc |= (nibble(x)<<shift);
	 shift+=7;
	 x = s.read();
       }

       /* Check the sign first to handle 2's complement asymmetry */
       if(is_neg_bit_set(x)) {
	 acc = -acc;
	 acc -= (mask_neg(x) << shift);
       } else {
	 acc += (mask_neg(x) << shift);
       }
     } catch (IOException e){
       die("Error reading int");
     }
     return acc;   
  }

  /* long */
  public static void write_java_long(long x,OutputStream s) {
    try {
      boolean  is_neg  =  (x < 0) ;      
      
      if (x == Long.MIN_VALUE) {
	/* handle 2's complement asymmetry */
	s.write(set_continue(Math.abs(Long.MIN_VALUE % 128L)));
	x = Math.abs(Long.MIN_VALUE / 128L); 
      } else {
	x = Math.abs(x);
      }
      
      while( x > 63L) {
	s.write(set_continue(nibble(x)));
	x >>= 7;
      }

      if(is_neg) { s.write(set_neg(x)); }
      else { s.write((int)x); }

    } catch (IOException e) {
      die("Error writing long");
    }
  }


  public static long read_java_long(InputStream s) {
    long acc = 0;
    int shift = 0;
    int x;

     try {
       x = s.read();
       while(is_continue_bit_set(x)) {
	 acc |= (nibble(x)<<shift);
	 shift+=7;
	 x = s.read();
       }

       /* Check the sign first to handle 2's complement asymmetry */
       if(is_neg_bit_set(x)) {
	 acc = -acc;
	 acc -= (mask_neg(x) << shift);
       } else {
	 acc += (mask_neg(x) << shift);
       }
     } catch (IOException e){
       die("Error reading long");
     }
     return acc;   
  }

  public static void write_java_double(double x, OutputStream s) {
    die("unimplemented");
  }

  public static double read_java_double(InputStream s) {
    die("unimplemented");
    return 1.0;
  }

  public static void write_java_BigInteger(BigInteger x, OutputStream s) {
    die("unimplemented");
  }

  public static BigInteger read_java_BigInteger(InputStream s) {
    die("unimplemented");
    return null;
  }


  public static void die() { die("Pkl error");  } 
  public static void write_tag(int x, OutputStream s) { write_java_int(x,s); }
  public static int read_tag(InputStream s) {  return read_java_int(s); }

  public static void write_int(int x, OutputStream s) { write_java_int(x,s); }
  public static int read_int(InputStream s) {  return read_java_int(s); }

  public static void write_String(String x,OutputStream s) {
    int sz = x.length();
    int i = 0;
    try {
      write_tag(sz,s); 
      while(i < sz) {
	s.write((byte)x.charAt(i++));
      }
    } catch (IOException e){
      die("Error writing String");
    }
  }

 public static String read_String(InputStream s) {

    
    int sz  = read_tag(s);
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
    return new identifier(read_String(s));
  }

  public static void write_identifier(identifier x,OutputStream s) {
    write_String(x.toString(),s);
  }



  public static void write_int_option(int_option i, OutputStream s) {
    if(i!=null) {
      write_tag(1,s);
      write_int(i.x,s);
    } else {
      write_int(0,s);
    }
  }

  public static int_option read_int_option(InputStream s) {
    if(read_int(s)!=0) {
      return new int_option(read_int(s));
    } else {
      return null;
    }
  }

  public static void write_String_option(String_option i, 
					       OutputStream s) {
    if(i!=null) {
      write_tag(1,s);
      write_String(i.x,s);
    } else {
      write_tag(0,s);
    }
  }

  public static String_option read_String_option(InputStream s) {
    if(read_tag(s)!=0) {
      return new String_option(read_String(s));
    } else {
      return null;
    }
  }

  public static void write_identifier_option(identifier_option i, 
					      OutputStream s) {
    if(i!=null) {
      write_tag(1,s);
      write_identifier(i.x,s);
    } else {
      write_tag(0,s);
    }
  }

  public static identifier_option read_identifier_option(InputStream s) {
    if(read_tag(s)!=0) {
      return new identifier_option(read_identifier(s));
    } else {
      return null;
    }
  }

  public static int_list read_int_list(InputStream s) {
    int_list t;
    int t1;
    int_list t2;
    t1 = read_tag(s);
    if(t1 != 0)
      t = new int_list(read_int(s), null);
    else
      return null;
    t1 = t1 - 1;
    t2 = t;
    while(t1 != 0) {
	t2.tail = new int_list(read_int(s), null);
	t2 = t2.tail;
	t1 = t1 - 1;
      }
    return t;
  }

  
  public static void write_int_list(int_list x, OutputStream s) {
    int t1;
    int_list t2;
    t1 = 0;
    t2 = x;

    while(t2 != null) {
      t2 = t2.tail;
      t1 = t1 + 1;
    }
    write_tag(t1, s);
    t2 = x;
    while(t1 != 0) {
      write_int(t2.head, s);
      t2 = t2.tail;
      t1 = t1 - 1;
    }
  }

  public static String_list read_String_list(InputStream s) {
    String_list t;
    int t1;
    String_list t2;
    t1 = read_tag(s);
    if(t1 != 0)
      t = new String_list(read_String(s), null);
    else
      return null;
    t1 = t1 - 1;
    t2 = t;
    while(t1 != 0) {
	t2.tail = new String_list(read_String(s), null);
	t2 = t2.tail;
	t1 = t1 - 1;
      }
    return t;
  }

  
  public static void write_String_list(String_list x, 
						 OutputStream s) {
    int t1;
    String_list t2;
    t1 = 0;
    t2 = x;

    while(t2 != null) {
      t2 = t2.tail;
      t1 = t1 + 1;
    }
    write_tag(t1, s);
    t2 = x;
    while(t1 != 0) {
      write_String(t2.head, s);
      t2 = t2.tail;
      t1 = t1 - 1;
    }
  }
  public static identifier_list read_identifier_list(InputStream s) {
    identifier_list t;
    int t1;
    identifier_list t2;
    t1 = read_tag(s);
    if(t1 != 0)
      t = new identifier_list(read_identifier(s), null);
    else
      return null;
    t1 = t1 - 1;
    t2 = t;
    while(t1 != 0) {
	t2.tail = new identifier_list(read_identifier(s), null);
	t2 = t2.tail;
	t1 = t1 - 1;
      }
    return t;
  }

  public static void write_identifier_list(identifier_list x, OutputStream s) {
    int t1;
    identifier_list t2;
    t1 = 0;
    t2 = x;

    while(t2 != null) {
      t2 = t2.tail;
      t1 = t1 + 1;
    }
    write_tag(t1, s);
    t2 = x;
    while(t1 != 0) {
      write_identifier(t2.head, s);
      t2 = t2.tail;
      t1 = t1 - 1;
    }
  }

}

