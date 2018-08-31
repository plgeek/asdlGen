{- 

HaskellBase.hs: Pickling/unpickling primitives for Haskell

Fermin Reig Galilea
University of Glasgow
http://www.dcs.gla.ac.uk/~reig/

13-1-1998

-}

module HaskellBase(Identifier, Handle, Int, String, IO, 
		   Maybe, return, 
	           write_string, read_string, write_identifier, 
		   read_identifier, write_list, read_list, write_option,
		   read_option, 
                   write_int, read_int, 
	           write_tag, read_tag, die)  where

import Prelude
import IO
import Char (chr, ord)
type Identifier = String
type Outstream = Handle
type Instream = Handle

-- Bit manipulation routines. 

-- The semantics of 'andb' 'orb' in this code is not that of 
-- the real bitwise operations 'and' 'or' if used with arbitrary arguments
-- However, in this code we always use 
-- n `andb` "11...11"  (mask with all 1's in binary)
-- and that is the same as n mod (11...11+1)
-- `orb` is always used to set bits that were previously 0, so 
-- adding gives the same value


andb :: Int -> Int -> Int
x `andb` y = x `mod` (y+1)

orb :: Int -> Int -> Int
orb = (+)

shiftl :: Int -> Int -> Int
n `shiftl` s = n * (2^s)

shiftr :: Int -> Int -> Int
n `shiftr` s = n `div` (2^s)


write_int :: Int -> Handle -> IO ()
write_int n s = loop (abs n)
	        where 
		loop x | x <= 63   = hPutChar s (chr (finish (n<0) x))
		       | otherwise = do 
				      hPutChar s (chr (nibble x))
				      loop (x `shiftr` 7)
	        nibble n = ((n `andb` 0x7f) `orb` 0x80) `andb` 0xff
		finish False n = n `andb` 255
		finish True  n = (n `orb` 0x40) `andb` 0xff

write_tag :: Int -> Handle -> IO ()
write_tag = write_int

write_string :: String -> Handle -> IO ()
write_string str s = do
		      write_int (length str) s
		      hPutStr s str

read_string :: Handle -> IO String
read_string s = do
		  sz  <- read_int s
		  str <- readN s sz
		  return str 


write_identifier :: Identifier -> Handle -> IO ()
write_identifier = write_string

read_int :: Handle -> IO Int
read_int s  
 = do {
     c <- hGetChar s;
     loop (ord c) 0 0
   } 
   where
	loop n acc shift = 
	  if (continue_bit_set n) then
	    do {
	      c <- hGetChar s;
	      loop ((ord c) `andb` 255) 
		    (acc `orb` ((n `andb` 0x7f) `shiftl` shift)) 
		    (shift+7)
	    }
	  else 
	    let acc' = acc `orb` ((n `andb` 0x3f) `shiftl` shift) 
	      in 
	      return (if neg_bit_set n then (negate acc') else  acc')

	continue_bit_set w = odd (w `shiftr` 7)
	neg_bit_set w      = odd (w `shiftr` 6)

read_tag :: Handle -> IO Int
read_tag = read_int

read_identifier :: Handle -> IO Identifier
read_identifier = read_string 

readN :: Handle -> Int -> IO [Char]
readN s n = if n == (0::Int) then return ""
	              else do {c  <- hGetChar s;
			       cs <- readN s (n-1);
			       return (c:cs)}

write_list :: (a -> Handle -> IO b) -> [a] -> Handle -> IO ()
write_list f xs s = do 
		     write_int (length xs) s
		     sequence (map (\x -> f x s) xs)

write_option :: (a -> Handle -> IO ()) -> Maybe a -> Handle -> IO ()
write_option f Nothing s  = write_int 0 s
write_option f (Just x) s = do 
			      write_int 1 s
			      f x s

read_list :: (Handle -> IO a) -> Handle -> IO [a]
read_list f s = do
		 len <- read_int s
		 if len < 0 then error "neg size" else 
		    accumulate (replicate len (f s))
	
read_option :: (Handle -> IO a) -> Handle -> IO (Maybe a)
read_option f s = do
		   n <- read_int s
		   case n of
		      0 -> return Nothing
		      1 -> do { o <- f s ; return (Just o)}
		      _ -> die

picklerError :: IOError
picklerError = userError "Pickler error"
 
die :: IO a
die = fail picklerError





