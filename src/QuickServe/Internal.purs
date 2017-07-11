module QuickServe.Internal where

import Prelude

import Data.Maybe (fromMaybe')
import Data.StrMap (lookup)
import Partial.Unsafe (unsafeCrashWith)
import Type.Row (class RowToList, Cons, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

data LProxy (l :: RowList) = LProxy

foreign import data RecordOf :: RowList -> Type

unsafeGet :: forall l a r. String -> RecordOf (Cons l a r) -> a
unsafeGet s r =
  fromMaybe'
    (\_ -> unsafeCrashWith ("unsafeGet: missing key " <> show s))
    (lookup s (unsafeCoerce r))

rowToList :: forall proxy r l. RowToList r l => proxy r -> LProxy l
rowToList _ = LProxy
