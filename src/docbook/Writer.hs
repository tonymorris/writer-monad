module Main where

type LOG = [String]

data Writer a =
  Writer {
    log' :: LOG
  , a :: a
  }

instance Functor Writer where
  fmap f (Writer log a) = 
    Writer log (f a)

instance Monad Writer where
  return =
    Writer []
  Writer log a >>= f =
    let Writer log2 b = f a
    in Writer (log ++ log2) b

nolog ::
  a
  -> Writer a
nolog =
  return

withlog ::
  a
  -> String
  -> Writer a
withlog a s =
  Writer (return s) a

withvaluelog ::
  a
  -> (a -> String)
  -> Writer a
withvaluelog a log =
  withlog a (log a)

withvaluelogs ::
  Show a =>
  a
  -> (String -> String)
  -> Writer a
withvaluelogs a log =
  withvaluelog a (log . show)

{-
> main' 41
Result: False
LOG
===
starting with 41
adding 7
switcheroo with 441
is even?
-}
main' k =
  let r = do a <- k `withvaluelogs` ("starting with " ++)
             b <- (a + 7) `withlog` "adding 7"
             c <- nolog (b * 3)
             d <- read (reverse (show c)) `withvaluelogs` ("switcheroo with " ++)
             e <- even d `withlog` "is even?"
             return e
  in mapM_ putStrLn ([
                       "Result: " ++ show (a r)
                     , "LOG"
                     , "==="
                     ] ++ log' r)