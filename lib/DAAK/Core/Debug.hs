-----------------------------------------------------------------------------
--
-- Module      : Debug
-- Copyright   : Armin Kazmi (2015)
-- License     : MIT
--
-- Maintainer  : Armin Kazmi
-- Stability   : experimental
-- Portability : portable
--
-- |
--
-----------------------------------------------------------------------------

module DAAK.Core.Debug where

import           Debug.Trace

globalDebug = True


traceShowSId :: (Show a) => String -> a -> a
traceShowSId s a = if globalDebug then trace (s ++ show a) a else a
