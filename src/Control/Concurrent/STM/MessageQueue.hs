{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.STM.MessageQueue
  ( TSource,
    TSink,
    matchSource,
    tryMatchSource,
    readSource,
    tryReadSource,
    writeSink,
    newMessageQueue,
    newMessageQueueIO,
  )
where

import Control.Applicative (optional)
import Control.Concurrent.STM
import Control.Monad.Fix (fix)
import Data.Maybe (fromJust, isJust)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq

data TSource a = TSource
  { incoming :: {-# UNPACK #-} !(TQueue a)
  , leftovers :: {-# UNPACK #-} !(TVar (Seq a))
  }

newtype TSink a = TSink {outgoing :: TQueue a}

newMessageQueue :: STM (TSink a, TSource a)
{-# INLINE newMessageQueue #-}
newMessageQueue = do
  q <- newTQueue
  v <- newTVar Seq.empty
  pure (TSink q, TSource q v)

newMessageQueueIO :: IO (TSink a, TSource a)
{-# INLINE newMessageQueueIO #-}
newMessageQueueIO = atomically newMessageQueue

writeSink :: TSink a -> a -> STM ()
{-# INLINE writeSink #-}
writeSink = writeTQueue . outgoing

readSource :: TSource a -> STM a
{-# INLINE readSource #-}
readSource (TSource inc left) = do
  readTVar left >>= \case
    a :<| as -> a <$ writeTVar left as
    Empty -> readTQueue inc

tryReadSource :: TSource a -> STM (Maybe a)
{-# INLINE tryReadSource #-}
tryReadSource = optional . readSource

matchSource :: TSource a -> (a -> Maybe b) -> STM b
matchSource (TSource inc left) pr = do
  lefts <- readTVar left
  case Seq.breakl (isJust . pr) lefts of
    (sq, l :<| seq') -> do
      writeTVar left $ sq >< seq'
      pure $ fromJust $ pr l
    _ -> fix $ \self -> do
      q <- readTQueue inc
      case pr q of
        Just p -> pure p
        Nothing ->
          modifyTVar' left (Seq.|> q) >> self

tryMatchSource :: TSource a -> (a -> Maybe b) -> STM (Maybe b)
{-# INLINE tryMatchSource #-}
tryMatchSource src pr = optional $ matchSource src pr
