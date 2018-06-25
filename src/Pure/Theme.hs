{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, PatternSynonyms, ViewPatterns #-}
module Pure.Theme
  ( Namespace(..)
  , Themeable(..)
  , themed
  , pattern Theme
  ) where

-- from pure
import Pure

-- from pure-css
import Pure.Data.CSS

-- from base
import Control.Arrow ((&&&))
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.Typeable
import System.IO.Unsafe

newtype NS = NS { getNamespace :: Txt }
instance Default NS where
  def = NS ""

class Typeable t => Themeable t where
  themeWrittenRef :: t -> IORef Bool
  themeWrittenRef _ = unsafePerformIO $ newIORef False
  namespace :: t -> NS
  namespace _ = def
  prefix :: t -> Txt
  prefix ta = getNamespace (namespace ta) <> "-" <> toTxt (tyCon (undefined :: t))
  theme :: Txt -> t -> CSS ()
  theme _ _ = return ()

{-# NOINLINE addTheme #-}
addTheme :: forall t. (Typeable t, Themeable t) => Txt -> t -> ()
addTheme pre t = unsafePerformIO $ do
  let tc = toTxt (tyCon t)
      p = "." <> pre
  tw <- atomicModifyIORef' (themeWrittenRef t) $ \b -> (True,b)
  unless tw $ do
    inject Pure.head (Attribute "data-pmui-theme" tc $ css (theme p t))

themed_ :: forall b t. (Typeable t, Themeable t, HasFeatures b) => t -> b -> (Txt,b)
themed_ t b = 
  let pr = Proxy :: Proxy b
      pre = prefix t
  in addTheme pre t `seq` (pre,Class pre b)

themed :: (Typeable t, Themeable t, HasFeatures b) => t -> b -> b
themed t b = snd $ themed_ t b

pattern Theme :: (HasFeatures b, Typeable t, Themeable t) => t -> b -> b
pattern Theme t b <- (const undefined &&& id -> (t,b)) where
  Theme t b = themed t b
