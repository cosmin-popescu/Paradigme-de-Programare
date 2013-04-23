module Parser where
module Parser where

import Data.Char

-- The slightly extendend parser from Simon Thompson: 
-- Haskell - The Craft of Functional Programming, Addison-Wesley 1997

type Parse t u = [t] -> [(u,[t])]

fail :: Parse t u
fail inp = []

succeed :: u -> Parse t u
succeed val inp = [(val,inp)]

-- Test for a token
token :: Eq t => t -> Parse t t
token t [] = []
token t (a:x)
   | t==a  = [(t,x)]
   | otherwise = []

-- Test for a list of tokens
lToken :: Eq t => [t] -> Parse t [t]
lToken tokens input = search [] tokens input where
    search res [] input = [(reverse res,input)]
    search _   _  [] = []
    search res (t:tokens) (x:rest_in) = 
               if t==x then search (x:res) tokens rest_in else []

-- Test for an input prefix that makes true a predicate p
spot :: (t->Bool) -> Parse t t
spot p [] = []
spot p (a:x)
   | p a = [(a,x)]
   | otherwise = []

-- Parser alternative
alt :: Parse t u -> Parse t u -> Parse t u
alt p1 p2 inp = (p1 inp) ++ (p2 inp)

-- Parser concatenation
infixr 5 >*>
(>*>) :: Parse t u -> Parse t v -> Parse t (u,v)
(>*>) p1 p2 inp = [((y,z),rem2) | (y,rem1) <- p1 inp, (z,rem2) <- p2 rem1]

-- Converting the result of a parser p
build :: Parse t u -> (u->v) -> Parse t v
build p f inp = [(f x, rem) | (x,rem) <- p inp]

-- The list of matches of the parser p
list :: Parse t u -> Parse t [u]
list p = (succeed []) `alt` ((p >*> list p) `build` convert) 
                       where convert (a,x) = a:x

-- The non-empty list of matches of the parser p
neList :: Parse t u -> Parse t [u]
neList p = (p >*> (list p)) `build` convert where convert (a,x) = a:x

-- The max list of matches of the parser p
maxList :: Parse t u -> Parse t [u]
maxList p inp = [last (list p inp)]

-- The max non-empty list of matches of the parser p
maxNeList :: Parse t u -> Parse t [u]
maxNeList p = (p >*> (maxList p)) `build` convert where convert (a,x) = a:x

{- variant
maxNeList p inp = case res of 
                       ([],_) -> []
                       _ -> res  where res = maxList p inp 
-}

-- Match a non-empty sequence of elements that satisfy a given predicate
spotWhile :: (t -> Bool) -> Parse t [t]
spotWhile p = maxNeList (spot p)

-- Match a possibly empty sequence of elements that satisfy a given predicate
spotWhile' :: (t -> Bool) -> Parse t [t]
spotWhile' p = maxList (spot p)

{- variant
spotWhile p inp = 
  case (spot p inp) of
     [] -> [([],inp)]
     [(x,res1)] -> let [(l,res2)] = spotWhile p res1 in [(x:l,res2)]
-}

-- Top level parser
parse :: Parse t u -> [t] -> u
parse p inp = case results of
                     [] -> error "Bad syntax"
                     (x:_) -> x  
              where results = [ res | (res,[]) <- p inp]
