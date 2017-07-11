## Module QuickServe.Internal

#### `LProxy`

``` purescript
data LProxy (l :: RowList)
  = LProxy
```

#### `RecordOf`

``` purescript
data RecordOf :: RowList -> Type
```

#### `unsafeGet`

``` purescript
unsafeGet :: forall l a r. String -> RecordOf (Cons l a r) -> a
```

#### `rowToList`

``` purescript
rowToList :: forall proxy r l. RowToList r l => proxy r -> LProxy l
```


