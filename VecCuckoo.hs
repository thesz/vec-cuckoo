{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module VecCuckoo where

import Prelude hiding (read)

import Control.Monad

import Data.Bits

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM

class Hashed a where

class MutableVector a where
	data MutableVectorType a	-- allows for unboxed data, if fancy.
	new :: Int -> IO (MutableVectorType a)
	read :: MutableVectorType a -> Int -> IO a
	write :: MutableVectorType a -> Int -> a -> IO ()

data Cuckoo k a = Cuckoo
	{ cuckooBucketShift		:: !Int
	, cuckooKeyStore		:: !(UVM.IOVector k)	-- these two of same length, which is power of 2.
	, cuckooDataStore		:: !(MutableVectorType a)
	}

class (Eq k, UVM.Unbox k) => CuKey k where
	hashes :: k -> (Int, Int)
	emptyValue :: k			-- should not be equal to any non-empty value. cfe, if you have indices, reserve an index.

newCuckoo :: (MutableVector a, CuKey k) => IO (Cuckoo k a)
newCuckoo = do
	keyStore <- UVM.replicate defaultSize emptyValue
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
		indexMask = (shiftR (UVM.length ks) bs) - 1
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
			k' <- UVM.read (cuckooKeyStore cuckoo) i
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
		Just i-> UVM.write (cuckooKeyStore cuckoo) i emptyValue

_grow :: forall a k . (MutableVector a, CuKey k) => Cuckoo k a -> IO (Cuckoo k a)
_grow c@(Cuckoo bs ks ds) = do
	pairs <- collectClean [] 0
	ks <- UVM.replicate ls2 emptyValue
	ds <- new ls2
	let	c' = Cuckoo bs ks ds
	forM_ pairs $ \(k,a) -> store c' k a	-- this is grow-avoiding.
	return c'
	where
		ls = UVM.length ks
		ls2 = 2 * ls
		collectClean :: [(k, a)] -> Int -> IO [(k, a)]
		collectClean acc i
			| i >= ls = return acc
			| otherwise = do
				k <- UVM.read ks i
				if k == emptyValue
					then collectClean acc (i+1)
					else do
						a <- read ds i
						collectClean ((k, a) : acc) (i+1)

-- |This will attempt to find a previously stored key, an empty slot or element to replace and
-- will grow table if needed 
store :: (MutableVector a, CuKey k) => Cuckoo k a -> k -> a -> IO (Cuckoo k a)
store cuckoo k a = do
	loop 0 0 cuckoo k a
	where
		attemptsBeforeGrow = 50
		loop grows attempts cuckoo k a
			| grows > 3 = error "table grown three times"
			| grows * attemptsBeforeGrow <= attempts = do
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
					k' <- UVM.read ks i
					if k' == k || k' == emptyValue
						then return (Just i)
						else loop is
		storeExchange attempts cuckoo k a = do
			mbI <- scan cuckoo k
			case mbI of
				Just i -> do
					UVM.write (cuckooKeyStore cuckoo) i k
					write (cuckooDataStore cuckoo) i a
					return Nothing
				Nothing -> do
					let	(h1, h2) = hashes k
						h = if odd attempts then h1 else h2
						bs = cuckooBucketShift cuckoo
						withinBucket = (div attempts 2) .&. (shiftL 1 bs - 1)
						ks = cuckooKeyStore cuckoo
						ds = cuckooDataStore cuckoo
						bucket = h .&. UVM.length ks - 1
						index = shiftL bucket bs + withinBucket
					k' <- UVM.read ks index
					UVM.write ks index k
					a' <- read ds index
					write ds index a
					return $ Just (k', a')

