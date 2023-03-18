-- VecCuckoo.hs
--
-- An implementation of cuckoo hash table, which allows for different modes of storage for elements, including unboxed arrays.
--
-- I, Serguey Zefirov, the author of the code, place it into a public domain.

{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module VecCuckoo where

import Prelude hiding (read, lookup, length)

import Control.Monad

import Data.Bits

import Data.IORef

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

-------------------------------------------------------------------------------
-- Classes

class MutableVector a where
	data MutableVectorType a	-- allows for unboxed data, if fancy.
	new :: Int -> IO (MutableVectorType a)
	read :: MutableVectorType a -> Int -> IO a
	write :: MutableVectorType a -> Int -> a -> IO ()
	length :: MutableVectorType a -> Int

class (Eq k, MutableVector k) => CuKey k where
	hashes :: k -> (Int, Int)
	emptyValue :: k			-- should not be equal to any non-empty value. cfe, if you have indices, reserve an index.

-------------------------------------------------------------------------------
-- Interface that may require tracking after store.

data Cuckoo k a = Cuckoo
	{ cuckooBucketShift		:: !Int
	, cuckooKeyStore		:: !(MutableVectorType k)	-- these two of same length, which is power of 2.
	, cuckooDataStore		:: !(MutableVectorType a)
	}

_emptyKeys :: CuKey k => Int -> IO (MutableVectorType k)
_emptyKeys len = do
	ks <- new len
	forM_ [0..len-1] $ \i -> write ks i emptyValue
	return ks

newCuckoo :: (MutableVector a, CuKey k) => IO (Cuckoo k a)
newCuckoo = do
	keyStore <- _emptyKeys defaultSize
	dataStore <- new defaultSize
	return $ Cuckoo
		{ cuckooBucketShift	= defaultBucketShift
		, cuckooKeyStore	= keyStore
		, cuckooDataStore	= dataStore
		}
	where
		defaultBucketShift = 2
		defaultStoreShift = 10
		defaultSize = shiftL 1 (defaultStoreShift + defaultBucketShift)

_ranges :: CuKey k => Cuckoo k a -> k -> ([Int], [Int])
_ranges (Cuckoo bs ks _) k = (range h1, range h2)
	where
		(h1, h2) = hashes k
		indexMask = (shiftR (length ks) bs) - 1
		range h = [shiftL (h .&. indexMask)  bs + i | i <- [0..shiftL 1 bs - 1]]

_scan :: CuKey k => Cuckoo k a -> k -> IO (Maybe Int)
_scan cuckoo k = do
	let	(r1, r2) = _ranges cuckoo k
	rr1 <- loop r1
	case rr1 of
		Nothing -> loop r2
		r -> return r
	where
		ks = cuckooKeyStore cuckoo
		loop :: [Int] -> IO (Maybe Int)
		loop [] = return Nothing
		loop (i:is) = do
			--putStrLn $ "_scan: reading from " ++ show i
			k' <- read (cuckooKeyStore cuckoo) i
			if k' == k
				then return (Just i)
				else if k == emptyValue then return Nothing else loop is

lookup :: (MutableVector a, CuKey k) => Cuckoo k a -> k -> IO (Maybe a)
lookup cuckoo k = do
	mbi <- _scan cuckoo k
	case mbi of
		Nothing -> return Nothing
		Just i -> Just <$> read (cuckooDataStore cuckoo) i

delete :: CuKey k => Cuckoo k a -> k -> IO ()
delete cuckoo k = do
	mbi <- _scan cuckoo k
	case mbi of
		Nothing -> return ()
		Just i-> write (cuckooKeyStore cuckoo) i emptyValue

_grow :: forall a k . (MutableVector a, CuKey k) => Cuckoo k a -> IO (Cuckoo k a)
_grow c@(Cuckoo bs ks ds) = do
	--putStrLn "growing"
	pairs <- collectClean [] 0
	ks <- _emptyKeys ls2
	ds <- new ls2
	let	c' = Cuckoo bs ks ds
	forM_ pairs $ \(k,a) -> store c' k a	-- this is grow-avoiding.
	return c'
	where
		ls = length ks
		ls2 = 2 * ls
		collectClean :: [(k, a)] -> Int -> IO [(k, a)]
		collectClean acc i
			| i >= ls = return acc
			| otherwise = do
				k <- read ks i
				if k == emptyValue
					then collectClean acc (i+1)
					else do
						a <- read ds i
						collectClean ((k, a) : acc) (i+1)

-- |This will attempt to find a previously stored key, an empty slot or element to replace and
-- will grow table if needed 
store :: (MutableVector a, CuKey k) => Cuckoo k a -> k -> a -> IO (Cuckoo k a)
store cuckoo k a = do
	--putStrLn $ "storing: " ++ show k
	loop 0 0 cuckoo k a
	where
		attemptsBeforeGrow = 50
		loop grows attempts cuckoo k a
			| grows > 3 = error "table grown three times"
			| attempts > 0 && grows * attemptsBeforeGrow <= attempts = do
				--putStrLn $ "failed at " ++ show k
				cuckoo' <- _grow cuckoo
				loop (grows + 1) attempts cuckoo' k a
			| otherwise = do
				mbVacated <- storeExchange attempts cuckoo k a
				case mbVacated of
					Just (k, a) -> loop grows (attempts + 1) cuckoo k a
					Nothing -> return cuckoo
		scan cuckoo k = do
			let	(r1, r2) = _ranges cuckoo k
			rr1 <- loop r1
			case rr1 of
				Nothing -> loop r2
				r -> return r
			where
				ks = cuckooKeyStore cuckoo
				loop :: [Int] -> IO (Maybe Int)
				loop [] = return Nothing
				loop (i:is) = do
					--putStrLn $ "reading in scan from " ++ show i
					k' <- read ks i
					--putStrLn $ "scan: read key " ++ show k' ++ " at " ++ show i
					if k' == k || k' == emptyValue
						then return (Just i)
						else loop is
		storeExchange attempts cuckoo k a = do
			mbI <- scan cuckoo k
			case mbI of
				Just i -> do
					write (cuckooKeyStore cuckoo) i k
					write (cuckooDataStore cuckoo) i a
					return Nothing
				Nothing -> do
					let	(h1, h2) = hashes k
						h = if odd attempts then h1 else h2
						bs = cuckooBucketShift cuckoo
						withinBucket = (div attempts 2) .&. (shiftL 1 bs - 1)
						ks = cuckooKeyStore cuckoo
						ds = cuckooDataStore cuckoo
						bucket = h .&. (shiftR (length ks) bs - 1)
						index = shiftL bucket bs + withinBucket
					--putStrLn $ "exchanging " ++ show index
					k' <- read ks index
					write ks index k
					a' <- read ds index
					write ds index a
					return $ Just (k', a')

-------------------------------------------------------------------------------
-- IORef-based interface, slightly easier to work with.

type CuckooRef k a = IORef (Cuckoo k a)

newCuckooRef :: (MutableVector a, CuKey k) => IO (CuckooRef k a)
newCuckooRef = newCuckoo >>= newIORef

lookupRef :: (MutableVector a, CuKey k) => CuckooRef k a -> k -> IO (Maybe a)
lookupRef cuckooRef k = do
	c <- readIORef cuckooRef
	lookup c k

deleteRef :: CuKey k => CuckooRef k a -> k -> IO ()
deleteRef cuckooRef k = do
	c <- readIORef cuckooRef
	delete c k

storeRef :: (MutableVector a, CuKey k) => CuckooRef k a -> k -> a -> IO ()
storeRef cuckooRef k a = do
	c <- readIORef cuckooRef
	c <- store c k a
	writeIORef cuckooRef c

-------------------------------------------------------------------------------
-- Handy MutableVector instances.

instance CuKey Int where
	hashes k = (k, mod k 65537 * 100001 + div k 65537 * 3333333)
	emptyValue = minBound

instance MutableVector Int where
	newtype MutableVectorType Int = MV_Int (UVM.IOVector Int)
	new n = MV_Int <$> UVM.new n
	read (MV_Int v) = UVM.read v
	write (MV_Int v) = UVM.write v
	length (MV_Int v) = UVM.length v

instance (MutableVector a, MutableVector b) => MutableVector (a, b) where
	data MutableVectorType (a, b) = MV_Tup2 !(MutableVectorType a) !(MutableVectorType b)
	new n = MV_Tup2 <$> new n <*> new n
	read (MV_Tup2 a b) i = (,) <$> read a i <*> read b i
	write (MV_Tup2 a b) i (x, y)  = write a i x >> write b i y
	length (MV_Tup2 a _) = length a


-------------------------------------------------------------------------------
-- Testing section.

{-
data DummyKey = DummyKey Int deriving (Show)

instance Eq DummyKey where
	DummyKey a == DummyKey b = a == b

instance MutableVector DummyKey where
	newtype MutableVectorType DummyKey = MV_DK (MutableVectorType Int)
	new n = MV_DK <$> new n
	read (MV_DK a) i = DummyKey <$> read a i
	write (MV_DK a) i (DummyKey x)  = write a i x
	length (MV_DK a) = length a

instance CuKey DummyKey where
	hashes (DummyKey a) = (a * 4096, (a + 1) * 4096)
	emptyValue = DummyKey minBound

t1 = do
	c <- newCuckoo :: IO (Cuckoo Int Int)
	lookup c 10 >>= print
	c <- store c 10 20
	lookup c 10 >>= print
	c <- store c 10 30
	lookup c 10 >>= print
	delete c 10
	lookup c 10 >>= print

t2 = do
	c <- newCuckooRef :: IO (CuckooRef DummyKey Int)
	forM_ [1..32] $ \i -> storeRef c (DummyKey i) i
	forM_ [1..32] $ \i -> do
		v <- lookupRef c (DummyKey i)
		print (i, v)
t = t2
-}
