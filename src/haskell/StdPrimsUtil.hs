module StdPrimsUtil(write_int, write_string, write_identifier, write_big_int,
                    read_int, read_string,  read_identifier, read_big_int,
	            sexp_wr_int, sexp_wr_string, sexp_wr_identifier, 
	            sexp_rd_int, sexp_rd_string, sexp_rd_identifier) where

import Prelude
import IO
import qualified StdPrims
import qualified StdPkl
import qualified SexpPkl
import qualified SexpLex

write_int        :: StdPrims.Int -> StdPkl.Outstream -> StdPkl.OutIO ()
write_string     :: StdPrims.String -> StdPkl.Outstream -> StdPkl.OutIO ()
write_identifier :: StdPrims.Identifier -> StdPkl.Outstream -> StdPkl.OutIO ()
write_big_int    :: StdPrims.Big_int -> StdPkl.Outstream -> StdPkl.OutIO ()

write_int = StdPkl.write_integral
write_string str s = do
		      StdPkl.write_tag (length str) s
		      hPutStr s str
write_identifier = write_string
write_big_int = StdPkl.write_integral

read_int        :: StdPkl.Instream -> StdPkl.OutIO StdPrims.Int
read_string     :: StdPkl.Instream -> StdPkl.OutIO StdPrims.String
read_identifier :: StdPkl.Instream -> StdPkl.OutIO StdPrims.Identifier
read_big_int    :: StdPkl.Instream -> StdPkl.OutIO StdPrims.Big_int

read_int = StdPkl.read_integral
read_string s = do
		  sz  <- StdPkl.read_tag s
		  str <- readN s sz
		  return str 
read_identifier = read_string 
readN :: StdPkl.Instream -> Int -> IO [Char]
readN s n = if n == (0::Int) then return ""
	              else do {c  <- hGetChar s;
			       cs <- readN s (n-1);
			       return (c:cs)}
read_big_int = StdPkl.read_integral

sexp_wr_int        :: StdPrims.Int -> SexpPkl.Outstream -> SexpPkl.OutIO ()
sexp_wr_string     :: StdPrims.String -> SexpPkl.Outstream -> SexpPkl.OutIO ()
sexp_wr_identifier :: StdPrims.Identifier -> SexpPkl.Outstream -> 
	                                                SexpPkl.OutIO ()
out_prim :: String -> (a -> SexpLex.Tok) -> a -> 
                      SexpPkl.Outstream -> SexpPkl.OutIO ()
out_prim t f x h = 
               do SexpPkl.wr_lp h
    	          SexpPkl.wr_sym t h
                  hPutStr h (SexpLex.toString (f x))
		  SexpPkl.wr_lp h
		  
sexp_wr_int = out_prim "int" SexpLex.INT 
sexp_wr_string = out_prim "string" SexpLex.STR 
sexp_wr_identifier = out_prim "identifier" SexpLex.STR

get_prim :: String -> SexpPkl.Instream -> SexpPkl.InIO SexpLex.Tok 
get_prim t s = do SexpPkl.rd_lp s
                  SexpPkl.rd_sym t s
		  do t <- SexpPkl.scanStream SexpLex.scan s
		     (case t of
		       Just t ->  do SexpPkl.rd_rp ()
			             return t
		       _ -> error "end of stream") 

sexp_rd_int        :: SexpPkl.Instream -> SexpPkl.InIO StdPrims.Int 
sexp_rd_string     :: SexpPkl.Instream -> SexpPkl.InIO StdPrims.String
sexp_rd_identifier :: SexpPkl.Instream -> SexpPkl.InIO StdPrims.Identifier

sexp_rd_int s = do t <- get_prim "int" s
                   (case t of 
                     SexpLex.INT i -> return i
		     _ -> error "expected int")

sexp_rd_string s = do t <- get_prim "string" s
                      (case t of 
                        SexpLex.STR str -> return str
		        _ -> error "expected str")

sexp_rd_identifier s = do t <- get_prim "identifier" s
                          (case t of 
			  SexpLex.STR str -> return str
			  _ -> error "expected identifier")



