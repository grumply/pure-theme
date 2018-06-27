{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, PatternSynonyms, ViewPatterns, TupleSections #-}
module Pure.Theme
  ( Themeable(..)
  , themed
  , pattern Theme
  ) where

-- from pure
import Pure

-- from pure-css
import Pure.Data.CSS

-- from pure-txt-trie
import Pure.Data.Txt.Trie as Trie

-- from base
import Control.Arrow ((&&&))
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.Typeable
import Data.Unique
import System.IO.Unsafe

import Data.Hashable
import Debug.Trace

{-# NOINLINE activeThemes #-}
activeThemes :: IORef (TxtTrie ())
activeThemes = unsafePerformIO $ newIORef Trie.empty

class Typeable t => Themeable t where
  namespace :: t -> Txt
  namespace _ = toTxt (tyCon (undefined :: t)) <> "_" <> toTxt (abs $ hash (typeOf (undefined :: t)))
  theme :: Txt -> t -> CSS ()
  theme _ _ = return ()

{-# NOINLINE addTheme #-}
addTheme :: Themeable t => Txt -> t -> ()
addTheme pre t = unsafePerformIO $ do
  let p = "." <> pre
  tw <- atomicModifyIORef' activeThemes $ \trie -> 
          if Trie.lookup pre trie == Just ()
            then (trie,True)
            else (Trie.insert pre () trie,False)
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
