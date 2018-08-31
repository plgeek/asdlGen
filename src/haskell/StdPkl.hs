{- 
Original Author
Fermin Reig Galilea
University of Glasgow
http://www.dcs.gla.ac.uk/~reig/

13-1-1998

Modified by Daniel Wang for new asdlGen naming convention.
-}

module StdPkl(Outstream, Instream, InIO, OutIO,
	      write_integral, read_integral,
              write_tag, read_tag,
	      write_list, read_list, 
              write_option,  read_option, die)  where

import Prelude
import IO
import Char (chr, ord)
type Outstream = Handle
type Instream = Handle
type InIO a = IO a 
type OutIO a = IO a 
-- Bit manipulation routines. 

-- The semantics of 'andb' 'orb' in this code is not that of 
-- the real bitwise operations 'and' 'or' if used with arbitrary arguments
-- However, in this code we always use 
-- n `andb` "11...11"  (mask with all 1's in binary)
-- and that is the same as n mod (11...11+1)
-- `orb` is always used to set bits that were previously 0, so 
-- adding gives the same value
--
-- There should be a version that uses the BITS library

andb :: (Integral a) => a -> a -> a
x `andb` y = x `mod` (y+1)

orb :: (Integral a) => a -> a -> a
orb = (+)

shiftl :: (Integral a) => a -> a -> a
n `shiftl` s = n * (2^s)

shiftr :: (Integral a) => a ->  a ->  a
n `shiftr` s = n `div` (2^s)


write_integral :: Integral a => a -> Handle -> IO ()
write_integral n s = loop (abs n)
	        where 
		loop x | x <= 63   = hPutChar s (chr (toInt(finish (n<0) x)))
		       | otherwise = do 
				      hPutChar s (chr (toInt(nibble x)))
				      loop (x `shiftr` 7)
	        nibble n = ((n `andb` 0x7f) `orb` 0x80) `andb` 0xff
		finish False n = n `andb` 255
		finish True  n = (n `orb` 0x40) `andb` 0xff


read_integral :: Integral a => Handle -> IO a
read_integral s  
 = do {
     c <- hGetChar s;
     loop (fromInt (ord c)) 0 0
   } 
   where
	loop n acc shift = 
	  if (continue_bit_set n) then
	    do {
	      c <- hGetChar s;
	      loop (fromInt(ord c) `andb` 255) 
		    (acc `orb` ((n `andb` 0x7f) `shiftl` shift)) 
		    (shift+7)
	    }
	  else 
	    let acc' = acc `orb` ((n `andb` 0x3f) `shiftl` shift) 
	      in 
	      return (if neg_bit_set n then (negate acc') else  acc')

	continue_bit_set w = odd (w `shiftr` 7)
	neg_bit_set w      = odd (w `shiftr` 6)

write_tag :: Int -> Handle -> IO () 
write_tag = write_integral
read_tag :: Handle -> IO Int
read_tag = read_integral

write_list :: (a -> Handle -> IO b) -> [a] -> Handle -> IO ()
write_list f xs s = do 
		     write_tag (length xs) s
		     sequence_ (map (\x -> f x s) xs)

write_option :: (a -> Handle -> IO ()) -> Maybe a -> Handle -> IO ()
write_option f Nothing s  = write_tag 0 s
write_option f (Just x) s = do 
			      write_tag 1 s
			      f x s

read_list :: (Handle -> IO a) -> Handle -> IO [a]
read_list f s = do
		 len <- read_tag s
		 if len < 0 then error "neg size" else 
		    sequence (replicate len (f s))
	
read_option :: (Handle -> IO a) -> Handle -> IO (Maybe a)
read_option f s = do
		   n <- read_tag s
		   case n of
		      0 -> return Nothing
		      1 -> do { o <- f s ; return (Just o)}
		      _ -> die ()

die :: () -> a
die = error "StdPkl"





