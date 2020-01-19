module Data.AppendList where

-- |A list, appending is O(1) instead of prepending.
newtype AppendList a = AppendList [a]

-- |Empty AppendList.
nil :: AppendList a
nil = AppendList []

-- |Append to an AppendList.
append :: AppendList a -> a -> AppendList a
append (AppendList xs) x = AppendList (x:xs)

-- |Get a list from an AppendList.
unwrapAppendList :: AppendList a -> [a]
unwrapAppendList (AppendList xs) = reverse xs

instance Semigroup (AppendList a) where
    AppendList xs <> AppendList ys = AppendList (ys <> xs)

instance Monoid (AppendList a) where
    mempty = nil
