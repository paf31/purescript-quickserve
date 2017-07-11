## Module QuickServe.Internal

#### `LProxy`

``` purescript
data LProxy (l :: RowList)
  = LProxy
```

#### `get`

``` purescript
get :: forall l a r1 r2. IsSymbol l => RowCons l a r1 r2 => SProxy l -> {  | r2 } -> a
```

Get a property from a record by providing a label as a type-level string.

Note: the `RowCons` constraint makes this operation safe.

#### `rowToList`

``` purescript
rowToList :: forall proxy r l. RowToList r l => proxy r -> LProxy l
```


