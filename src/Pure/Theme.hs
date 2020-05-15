{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, PatternSynonyms, ViewPatterns, TupleSections, ExistentialQuantification, TypeApplications, AllowAmbiguousTypes, RoleAnnotations #-}
module Pure.Theme
  ( Theme(..)
  , pattern Themed
  , hasTheme
  , themed
  , themedWith
  , subtheme
  , SomeTheme(..)
  , mkSomeTheme
  , someThemed
  , module Pure.Data.CSS
  , module Pure.Data.Styles
  , Monoid(..)
  , void
  , for_
  , traverse_
  , for
  , traverse
  , (&)
  ) where

-- from pure
import Pure

-- from pure-css
import Pure.Data.CSS hiding (Namespace)

-- from pure-styles
import Pure.Data.Styles

-- from pure-txt-trie
import Pure.Data.Txt.Trie as Trie

-- from base
import Control.Arrow ((&&&))
import Control.Monad
import Data.Foldable
import Data.Function ((&))
import Data.Traversable
import Data.IORef
import Data.Monoid
import Data.Typeable
import Data.Unique
import System.IO.Unsafe

import Data.Hashable
import Debug.Trace

{-# NOINLINE activeThemes #-}
activeThemes :: IORef TxtSet
activeThemes = unsafePerformIO $ newIORef Trie.empty

qualifiedNamespace :: forall t. Typeable t => Txt
qualifiedNamespace = 
  let pto  = typeOf (undefined :: t)
      th   = abs (hash pto)
      trtc = typeRepTyCon pto
  in toTxt (show trtc <> "_" <> show th)

newtype Namespace t = Namespace Txt
type role Namespace nominal

class Typeable t => Theme t where
  namespace :: Namespace t
  namespace = Namespace (qualifiedNamespace @t)

  theme :: forall t. Txt -> CSS ()
  theme _ = return ()

{-# NOINLINE addTheme #-}
addTheme :: forall t. Theme t => Txt -> ()
addTheme pre = unsafePerformIO $ do
  let p = "." <> pre
  tw <- atomicModifyIORef' activeThemes $ \trie ->
          if Trie.lookup pre trie == Just ()
            then (trie,True)
            else (Trie.insert pre () trie,False)
  unless tw $ inject Pure.head (Attribute "data-pure-theme" pre (css (theme @t p)))

hasTheme :: forall t b. (Theme t, HasFeatures b) => b -> Bool
hasTheme (Classes cs b) = let Namespace t = namespace @t in t `elem` cs

themedWith :: forall t b. (Theme t, HasFeatures b) => Namespace t -> b -> b
themedWith _ = Themed @t

themed :: forall t b. (Theme t, HasFeatures b) => t -> b -> b
themed _ = Themed @t

pattern Themed :: forall t b. (HasFeatures b, Theme t) => b -> b
pattern Themed b <- (hasTheme @t &&& id -> (True,b)) where
  Themed b =
    let Namespace pre = namespace @t
    in addTheme @t pre `seq` Class pre b

subtheme :: forall t. Theme t => Txt
subtheme = let Namespace t = namespace @t in "." <> t

data SomeTheme = forall t. Theme t => SomeTheme (Namespace t)

mkSomeTheme :: forall t. Theme t => SomeTheme
mkSomeTheme = SomeTheme (namespace @t)

someThemed :: (HasFeatures b) => SomeTheme -> b -> b
someThemed (SomeTheme ns) = themedWith ns