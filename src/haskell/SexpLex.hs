module SexpLex(Tok(..),scan,toString,)  where

data Tok =
 LP | RP 
 | INT Int
-- TODO | BIGINT Integer
 | SYM String
 | STR String deriving (Eq,Show,Read)

-- TODO handle string escapes properly...
toString :: Tok -> String
toString LP = "("
toString RP = ")"
toString (INT i) = show i
toString (SYM s) = s++" "
toString (STR s) = (show s) -- escape characters corectly

eatws :: String -> String
eatws (x:xs) =  if isSpace x then eatws xs else x:xs

scan_int :: String -> Maybe (Int,String)
scan_int s = case (reads s) of
             [(i,s)] -> Just (i,s)
	     _ -> Nothing 	     

scan_str :: String -> Maybe (String,String)
scan_str s = case (reads ('"':s)) of
             [(i,s)] -> Just (i,s)
	     _ -> Nothing 	     

scan_sym :: String -> Maybe (String,String)
scan_sym [] = Nothing
scan_sym (c:s) = if isAlpha c then
                 Just (span (\x -> (isAlphaNum x) || x == '_') (c:s))
                 else Nothing
scan_tok  :: String -> Maybe (Tok,String)
scan_tok  [] = Nothing
scan_tok  ('(':s) = Just (LP,s)
scan_tok  (')':s) = Just (RP,s)
scan_tok  ('"':s) = case (scan_str s) of
                    Just(str,s) -> Just (STR str,s)
	            Nothing -> Nothing

scan_tok  s = case (scan_int s) of
	      Just (i,s) ->  Just (INT i,s)
	      Nothing -> case (scan_sym s) of
	                 Just (str,s) -> Just (SYM str,s)
    	                 Nothing -> Nothing

scan      :: String ->  Maybe (Tok,String)
scan s = scan_tok (eatws s)


