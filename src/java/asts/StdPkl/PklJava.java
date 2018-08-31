package asts.StdPkl;
import java.io.*;
import java.math.BigInteger;

public class PklJava {

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

  public static void die(String s) {
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

  public static void write_java_math_BigInteger(BigInteger x, OutputStream s) {
    die("unimplemented");
  }

  public static BigInteger read_java_math_BigInteger(InputStream s) {
    die("unimplemented");
    return null;
  }

  public static void die() { die("Pkl error");  } 

}


