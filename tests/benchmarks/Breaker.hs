-- Estimate the time difference between creating a breaker.

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.Text as T
import Data.Text.IO as T
import Data.Text.ICU.Break as IO
import Data.Text.ICU as ICU
import System.Environment

consume b = go
  where
    go = do
      m <- next b
      case m of
        Nothing -> return ()
        Just _ -> go

manyBreakers (t:ts) = do
  b <- IO.breakWord "en" t
  consume b
  manyBreakers ts
manyBreakers _ = return ()

oneBreaker ts = do
  b <- IO.breakWord "en" ""
  forM_ ts $ \t -> do
    setText b t
    consume b

cloneBreakers ts = do
  b <- IO.breakWord "en" ""
  forM_ ts $ \t -> do
    b' <- clone b
    setText b' t
    consume b'

pureBreaker ts = do
  let b = ICU.breakWord "en"
  forM_ ts $ \t -> length (breaks b t) `seq` return ()

main = do
  (kind:files) <- getArgs
  let act = case kind of
              "one" -> oneBreaker
              "many" -> manyBreakers
              "clone" -> cloneBreakers
              "pure" -> pureBreaker
  forM_ files $ \f -> T.readFile f >>= act . T.lines
