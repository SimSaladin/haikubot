{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- File: Plugins.hs
-- Creation Date: Aug 05 2012 [05:38:41]
-- Last Modified: Aug 05 2012 [22:20:38]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | The plugin interface.
module Plugins
  ( Handler
  , Plugin(..)
  , Connection
  , IrcMessage(..)
  , Shared
  , share
  , Result(..)
  ) where

import Control.Monad.Reader
import Network.SimpleIRC
import Handler (Handler, Shared, share, Plugin(..), Connection, Result(..))

