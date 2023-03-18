# Vector-based cuckoo hash table for Haskell

XXX: Close to completion, but not tested.

The code is in public domain, do whatever you want.

The hash table here requires keys to implement CuKey class interface which requires Data.Vector.Unboxed.Unbox implementation. This is so that keys can be stored in an unboxed array.

The storage for elements is slightly more complicated. Elements should have MutableVector instance implemented, the MutableVector can be boxed or unboxed mutable vector of sorts. Boxed vectors are good for storing complex data (sets, maps, lists or vectors), unboxed vectors provide quick access and less memory consumption.
