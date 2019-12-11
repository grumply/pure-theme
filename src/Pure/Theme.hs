{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, PatternSynonyms, ViewPatterns, TupleSections, ExistentialQuantification, TypeApplications #-}
module Pure.Theme
  ( Themeable(..)
  , Namespace(..)
  , themed
  , pattern Theme
  , SomeT(..)
  , module Pure.Data.CSS
  , void
  , for_
  , traverse_
  , for
  , traverse
  ) where

-- from pure
import Pure

-- from pure-css
import Pure.Data.CSS hiding (Namespace)

-- from pure-txt-trie
import Pure.Data.Txt.Trie as Trie

-- from base
import Control.Arrow ((&&&))
import Control.Monad
import Data.Foldable
import Data.Traversable
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

pTyCon :: Typeable t => Proxy t -> TyCon
pTyCon = typeRepTyCon . pTypeOf

pTypeOf :: forall t. Typeable t => Proxy t -> TypeRep
pTypeOf _ = typeOf (undefined :: t)

typeHash :: forall t. Typeable t => Namespace t
typeHash =
  Namespace $
       toTxt (show (pTyCon (Proxy :: Proxy t)))
    <> "_"
    <> toTxt (abs (hash (pTypeOf (Proxy :: Proxy t))))

newtype Namespace t = Namespace Txt

class Typeable t => Themeable t where
  namespace :: Namespace t
  namespace = typeHash

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

themed_ :: forall t b. (Themeable t, HasFeatures b) => t -> b -> (Txt,b)
themed_ t b =
  let Namespace pre = namespace @t
  in addTheme pre t `seq` (pre,Class pre b)

themed :: (Themeable t, HasFeatures b) => t -> b -> b
themed t b = snd $ themed_ t b

pattern Theme :: (HasFeatures b, Themeable t) => t -> b -> b
pattern Theme t b <- (const undefined &&& id -> (t,b)) where
  Theme t b = themed t b

data SomeT = forall t. Themeable t => SomeT t
