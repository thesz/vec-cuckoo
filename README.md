# Vector-based cuckoo hash table for Haskell

The code is in public domain, do whatever you want.

The hash table here requires keys to implement CuKey class interface which requires MutableVector implementation. This is so that keys can be stored in an array that is most appropriate, including boxed arrays, if needed. CuKey also requires a value that indicate emptyness and a way to extract hashes from a key.

Elements should have MutableVector instance implemented, the MutableVector can be boxed or unboxed mutable vector of sorts. Boxed vectors are good for storing complex data (sets, maps, lists or vectors), unboxed vectors provide quick access and less memory consumption.
