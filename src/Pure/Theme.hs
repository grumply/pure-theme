{-# LANGUAGE OverloadedStrings, FlexibleContexts, PatternSynonyms, ViewPatterns, TupleSections #-}
module Pure.Theme
  ( Themeable(..)
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
import Data.Unique
import System.IO.Unsafe

class Themeable t where
  {-# NOINLINE themeWrittenRef #-}
  themeWrittenRef :: t -> IORef Bool
  themeWrittenRef _ = unsafePerformIO $ newIORef False
  {-# NOINLINE namespace #-}
  namespace :: t -> Txt
  namespace _ = "t_" <> unsafePerformIO (toTxt . show . hashUnique <$> newUnique)
  theme :: Txt -> t -> CSS ()
  theme _ _ = return ()

{-# NOINLINE addTheme #-}
addTheme :: (Themeable t) => Txt -> t -> ()
addTheme pre t = unsafePerformIO $ do
  let p = "." <> pre
  tw <- atomicModifyIORef' (themeWrittenRef t) (True,)
  unless tw $ inject Pure.head (Attribute "data-pure-theme" pre (css (theme p t)))

themed_ :: (Themeable t, HasFeatures b) => t -> b -> (Txt,b)
themed_ t b = 
  let pre = namespace t
  in addTheme pre t `seq` (pre,Class pre b)

themed :: (Themeable t, HasFeatures b) => t -> b -> b
themed t b = snd $ themed_ t b

pattern Theme :: (HasFeatures b, Themeable t) => t -> b -> b
pattern Theme t b <- (const undefined &&& id -> (t,b)) where
  Theme t b = themed t b
