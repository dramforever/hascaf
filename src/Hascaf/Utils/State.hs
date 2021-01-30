module Hascaf.Utils.State where

import Control.Monad.State
import Lens.Micro.Platform

preserving
    :: MonadState s m
    => Lens' s x -> m a -> m a
preserving ls act = do
    original <- use ls
    res <- act
    ls .= original
    pure res
