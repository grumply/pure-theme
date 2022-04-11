module Pure.Theme
  ( Theme(..)
  , pattern Themed
  , pattern Customized
  , hasTheme
  , themed
  , themedWith
  , subtheme
  , at
  , at'
  , within
  , within'
  , embed
  , addTheme
  , addThemeClass
  , addSomeThemeClass
  , removeThemeClass
  , removeSomeThemeClass
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
import Pure.Data.CSS hiding (Namespace,empty,select,wrap)

-- from pure-lifted
import Pure.Data.Lifted as Lifted (JSV,Node,head)

-- from pure-styles
import Pure.Data.Styles

-- from pure-txt
import Pure.Data.Txt as Txt

-- from pure-txt-trie
import Pure.Data.Txt.Trie as Trie

-- from base
import Control.Arrow ((&&&))
import Control.Monad
import Data.Coerce
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
rep = Txt.filter nonQuote (Txt.intercalate "_" [go (typeRep (Proxy :: Proxy p)), toTxt th])
  where
    nonQuote x = x /= '\'' && x /= '\"'
    th = abs (hash (typeRep (Proxy :: Proxy p)))
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

{-# NOINLINE addThemeUnsafe #-}
addThemeUnsafe :: forall t. Theme t => ()
addThemeUnsafe = unsafePerformIO (addTheme @t)

addTheme :: forall t. Theme t => IO ()
addTheme = do
  let 
    Namespace pre = namespace @t
    p = "." <> pre
  tw <- atomicModifyIORef' activeThemes $ \trie ->
          if Trie.lookup pre trie == Just ()
            then (trie,True)
            else (Trie.insert pre () trie,False)
  unless tw $
    case css (theme @t p) of
      Children [ LazyView f a ] _ | TextView _ "" <- f a -> pure ()
      content -> inject Lifted.head (Attribute "data-pure-theme" pre content)

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
    in addThemeUnsafe @t `seq` Class pre b

subtheme :: forall t. Theme t => Txt
subtheme = let Namespace t = namespace @t in "." <> t

at :: forall t a. Theme t => CSS a -> CSS ()
at = is (subtheme @t)

at' :: forall t a. Theme t => CSS a -> CSS a
at' = is' (subtheme @t)

within :: forall t a. Theme t => CSS a -> CSS ()
within = void . within' @t

within' :: forall t a. Theme t => CSS a -> CSS a
within' block = do
  s <- scope
  rescope (subtheme @t <> " " <> s) block

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
    in addThemeUnsafe @t `seq` 
       addThemeUnsafe @(Custom t) `seq` 
       Class e (Class t b)

-- Adding and removing themes is designed to work with multiple theme sources.
-- That is, two components manually adding themes to a node can coexist without
-- removing each other's themes from the node. But, these themes are not managed
-- as themes applied with Themed; it is the user's responsibility to manage when
-- the theme is added and removed.

addSomeThemeClass :: SomeTheme -> Node -> IO ()
addSomeThemeClass st n = 
  case st of
    SomeTheme ns@(Namespace c) -> do
      addThemeFromNamespace ns
      addClass n c
  where
    addThemeFromNamespace :: forall t. Theme t => Namespace t -> IO ()
    addThemeFromNamespace _ = addTheme @t

removeSomeThemeClass :: SomeTheme -> Node -> IO ()
removeSomeThemeClass (SomeTheme (Namespace c)) n = removeClass n c

addThemeClass :: forall t. Theme t => Node -> IO ()
addThemeClass n = addTheme @t >> addClass n (Txt.tail (subtheme @t))

removeThemeClass :: forall t. Theme t => Node -> IO ()
removeThemeClass n = removeClass n (Txt.tail (subtheme @t))

#ifdef __GHCJS__
foreign import javascript unsafe
    "if ($1) { $1['data-pure-theme-list'] = ($1['data-pure-theme-list'] || '')['concat'](' ',$2); $1['classList']['add']($2) }" add_class_js :: Node -> Txt -> IO ()
#endif

addClass :: Node -> Txt -> IO ()
addClass n c =
#ifdef __GHCJS__
    add_class_js n c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "if ($1) { var l = ($1['data-pure-theme-list'] || '')['trim']()['split'](' '); var idx = l['findIndex'](function (p) { return p === $2; }); if (idx >= 0) { l.splice(idx,1) }; $1['data-pure-theme-list'] = l.join(' '); if (l['findIndex'](function (p) { return p === $2; }) < 0) { $1['classList']['remove']($2); } }" remove_class_js :: Node -> Txt -> IO ()
#endif

removeClass :: Node -> Txt -> IO ()
removeClass n c =
#ifdef __GHCJS__
    remove_class_js n c
#else
    return ()
#endif
