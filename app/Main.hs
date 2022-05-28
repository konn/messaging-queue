{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent.STM.MessageQueue
import Control.Lens
import Data.Dynamic
import Data.Dynamic.Lens
import RIO hiding (preview)

main :: IO ()
main = do
  (sink, src) <- newMessageQueueIO
  (sums, ()) <- summer src `concurrently` producer sink [0 .. 10]
  print sums

data Instruction = Int Int | End
  deriving (Show, Eq, Ord)

producer :: TSink Dynamic -> [Int] -> IO ()
producer sink = go
  where
    go [] = atomically $ do
      writeSink sink (toDyn "Hello")
      writeSink sink (toDyn End)
    go (i : is) = do
      atomically $ writeSink sink (toDyn ())
      atomically (writeSink sink (toDyn $ Int i))
      threadDelay (10 ^ (6 :: Int))
      go is

summer :: TSource Dynamic -> IO Int
summer src = loop 0
  where
    loop !acc =
      atomically (matchSource src (preview _Dynamic)) >>= \case
        End -> pure acc
        Int a -> do
          putStrLn $ "Int coming: " <> show a
          loop (acc + a)

--- >>> main
