{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, PatternSynonyms, ViewPatterns, TupleSections, ExistentialQuantification, TypeApplications, AllowAmbiguousTypes, RoleAnnotations #-}
module Pure.Theme
  ( Theme(..)
  , pattern Themed
  , pattern Customized
  , hasTheme
  , themed
  , themedWith
  , subtheme
  , embed
  , Custom(..)
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

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns

-- from pure-dom
import Pure.DOM (inject)

-- from pure-css
import Pure.Data.CSS hiding (Namespace,empty,select)

-- from pure-lifted
import Pure.Data.Lifted as Lifted (head)

-- from pure-styles
import Pure.Data.Styles

-- from pure-txt
import Pure.Data.Txt as Txt

-- from pure-txt-trie
import Pure.Data.Txt.Trie as Trie

-- from base
import Control.Arrow ((&&&))
import Control.Monad
import Data.Foldable
import Data.Function ((&))
import Data.Traversable
import Data.List as List
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

{-# INLINE rep #-}
rep :: forall p. (Typeable p) => Txt
rep = Txt.filter nonQuote (Txt.intercalate "_" [go (typeOf (undefined :: p)), toTxt th])
  where
    nonQuote x = x /= '\'' && x /= '\"'
    th = abs (hash (typeOf (undefined :: p)))
    go tr =
      let tc = toTxt (show (typeRepTyCon tr))
          trs = typeRepArgs tr
      in Txt.intercalate "_" (tc : fmap go trs)

newtype Namespace t = Namespace Txt
type role Namespace nominal

class Typeable t => Theme t where
  namespace :: Namespace t
  namespace = Namespace (rep @t)

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
  unless tw $ inject Lifted.head (Attribute "data-pure-theme" pre (css (theme @t p)))

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

embed :: forall sub. Theme sub => CSS ()
embed = let Namespace ns = namespace @sub in theme @sub ns

data SomeTheme = forall t. Theme t => SomeTheme (Namespace t)

mkSomeTheme :: forall t. Theme t => SomeTheme
mkSomeTheme = SomeTheme (namespace @t)

someThemed :: (HasFeatures b) => SomeTheme -> b -> b
someThemed (SomeTheme ns) = themedWith ns

data Custom a

pattern Customized :: forall t b. (HasFeatures b, Theme t, Theme (Custom t)) => b -> b
pattern Customized b <- (((&&) <$> hasTheme @(Custom t) <*> hasTheme @t) &&& id -> (True,b)) where
  Customized b =
    let Namespace t = namespace @t
        Namespace e = namespace @(Custom t)
    in addTheme @t t `seq` addTheme @(Custom t) e `seq` Class e (Class t b)

