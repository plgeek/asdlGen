module SexpPkl(Instream, Outstream, InIO, OutIO,
               die,readHandle, readString, scanStream, peekStream,
               wr_lp, wr_rp, wr_sym, wr_list, wr_option,
               rd_lp, rd_rp, get_sym, rd_sym, rd_list, rd_option) where 
import qualified SexpLex
import IO
type Outstream = Handle
type Instream = ()
type OutIO a = IO a 
data InIO a = InIO (String -> (a,String))

instance Monad InIO where
    InIO c1 >>= fc2 = 
         InIO (\s0 -> let (r,s1) = c1 s0 
                          InIO c2 = fc2 r in
                        c2 s1)
    return k = InIO (\s -> (k,s))		      

scanStream :: (String -> Maybe (a,String)) -> Instream -> InIO (Maybe a)
scanStream  scan s =  InIO (\s -> case scan s of 
                                  Just (x,s) -> (Just x,s)
				  Nothing -> (Nothing,s))  
          
peekStream :: (String -> Maybe (a,String)) -> Instream -> InIO (Maybe a)
peekStream  scan s =  InIO (\s -> case scan s of 
                                  Just (x,_) -> (Just x,s)
				  Nothing -> (Nothing,s))  
          
readString :: InIO a -> String -> (a,String)
readString (InIO f) s = f s

readHandle :: InIO a -> Handle ->  IO (a,String)
readHandle (InIO f) h = do s <- hGetContents h
                           return (f s)

wr_lp     :: Outstream -> OutIO ()
wr_rp     :: Outstream -> OutIO ()
wr_sym    :: String -> Outstream -> OutIO ()
wr_list   :: (a -> Outstream -> OutIO ()) -> [a] -> Outstream -> OutIO ()
wr_option :: (a -> Outstream -> OutIO ()) -> Maybe a -> Outstream -> OutIO ()

output_tok :: SexpLex.Tok -> Outstream -> OutIO ()
output_tok t h = hPutStr h (SexpLex.toString t)
wr_lp = output_tok SexpLex.LP
wr_rp = output_tok SexpLex.RP
wr_sym str = output_tok (SexpLex.SYM str)
wr_list f xs s = do 
                  wr_lp s
                  wr_sym "list" s
                  sequence_ (map (\x -> f x s) xs)
                  wr_rp s	
	  
wr_option f Nothing = wr_list f [] 
wr_option f (Just x) = wr_list f [x] 


rd_lp     :: Instream -> InIO ()
rd_rp     :: Instream -> InIO ()
rd_sym    :: String -> Instream -> InIO ()
get_sym   :: Instream -> InIO String
rd_list   :: (Instream -> InIO a) -> Instream -> InIO [a] 
rd_option :: (Instream -> InIO a) -> Instream -> InIO (Maybe a) 

expect_tok :: SexpLex.Tok -> Instream -> InIO ()
expect_tok t s = do x <- scanStream SexpLex.scan s
                    (case x of
		     Just t' -> if t == t' then return ()
                                else (error ("wrong token: "++(show t')++ " expected:"++(show t)))
 		     Nothing -> error ("bad token"))
		    		    
rd_lp = expect_tok SexpLex.LP
rd_rp = expect_tok SexpLex.RP
get_sym s = do x <- scanStream SexpLex.scan s
               (case x of 
                Just (SexpLex.SYM str) -> return str
                _ -> error ("Expected sym"))

rd_sym str s = do s <- get_sym s
                  if str == s then  return ()
	             else error ("Expected sym")

peek_rp :: Instream -> InIO Bool 
peek_rp s = do t <- peekStream SexpLex.scan s
               return (case t of 
	                Just SexpLex.RP -> True
	                _ -> False)
loop :: (Instream -> InIO a) -> Instream -> Bool -> InIO [a]
loop f s True = do rd_rp s
                   return []

loop f s False = do x <- f s
                    t <- peek_rp s 
                    xs <- loop f s t
                    return (x:xs)

rd_list f s = do rd_lp s 
                 rd_sym "list" s
                 t <- peek_rp s
                 xs <- loop f s t 
               	 return xs

rd_option f s = do x <- rd_list f s
                   (case x of
                     [x] -> return (Just x)
		     [] -> return Nothing
		     _ -> error "bad option") 
die :: () -> a
die = error "SexpPkl"