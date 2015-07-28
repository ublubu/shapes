module Utils.Utils where

import Control.Monad
import Control.Monad.State hiding (state)
import Data.Bits
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Word

with2 :: (Storable a, Storable b) => a -> b -> (Ptr a -> Ptr b -> IO c) -> IO c
with2 a b f = with a $ \a' -> with b (f a')


with3 :: (Storable a, Storable b, Storable c) =>
         a -> b -> c -> (Ptr a -> Ptr b -> Ptr c -> IO d) -> IO d
with3 a b c f = with a $ \a' -> with2 b c (f a')


with4 :: (Storable a, Storable b, Storable c, Storable d) =>
         a -> b -> c -> d -> (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO e) -> IO e
with4 a b c d f = with a $ \a' -> with3 b c d (f a')


withCAString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
withCAString2 a b op = withCAString a $ \a' -> withCAString b $ op a'


repeatUntilComplete :: IO Bool -> IO ()
repeatUntilComplete operation = do
    complete <- operation
    unless complete $ repeatUntilComplete operation


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer


infixl 4 ~>>
(~>>) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m b
(~>>) m f = m >>= lift . f


infixl 4 ~>~
(~>~) :: (Monad (t m), Monad m, MonadTrans t) => t m a -> (a -> m b) -> t m a
(~>~) m f = m >>= \x -> (lift . f) x >> return x


into :: (Monad m, MonadTrans t, MonadState b (t m)) => m a -> (a -> b -> b) -> t m b
into source f = lift source >>= modify . f >> get

infixr 9 <.
(<.) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(f <. g) x y = f (g x y)

pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)

pairAp :: (a -> b, c -> d) -> ((a, c) -> (b, d))
pairAp (f, g) (x, y) = (f x, g y)

pairFold :: Monoid m => (m, m) -> m
pairFold (a, b) = mappend a b

maybeChange :: a -> (a -> Maybe a) -> a
maybeChange x f = fromMaybe x (f x)

toMaybe :: Bool -> a -> Maybe a
toMaybe b x = if b then Just x else Nothing

maybeBranch :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Maybe (Either a a)
maybeBranch _ Nothing Nothing = Nothing
maybeBranch _ Nothing (Just x) = Just $ Right x
maybeBranch _ (Just x) Nothing = Just $ Left x
maybeBranch useLeft (Just x) (Just y) = if useLeft x y then Just $ Left x
                                        else Just $ Right y

takeIfAll :: (a -> Bool) -> [a] -> Maybe [a]
takeIfAll p [] = Just []
takeIfAll p (x:xs)
  | p x = fmap (x:) (takeIfAll p xs)
  | otherwise = Nothing

cycles :: [a] -> [[a]]
cycles xs = folds tail (cycle xs) xs

data Loop a = Loop { loopPrev :: Loop a
                   , loopVal :: a
                   , loopNext :: Loop a } deriving Show

loopify :: [a] -> Loop a
loopify [] = error "can't have an empty loop"
loopify v = let (first, last) = f last v first
            in first
  where f :: Loop a -> [a] -> Loop a -> (Loop a, Loop a)
        f prev [] next = (next, prev)
        f prev (x:xs) next = let (next', last') = f this xs next
                                 this = Loop prev x next'
                             in (this, last')

takeNext :: Int -> Loop a -> [Loop a]
takeNext = takeDir loopNext

takePrev :: Int -> Loop a -> [Loop a]
takePrev = takeDir loopPrev

takeDir :: (Loop a -> Loop a) -> Int -> Loop a -> [Loop a]
takeDir dir n x
  | n > 0 = x : takeDir dir (n - 1) (dir x)
  | n == 0 = []
  | n < 0 = error "cannot take fewer than 0"

folds :: (b -> b) -> b -> [a] -> [b]
folds _ _ [] = []
folds f a0 (_:xs) = a0 : folds f (f a0) xs

data Flipping a = Same a | Flip a deriving Show

-- TODO: write an iso for Flipping and Either
flipAsEither :: Flipping a -> Either a a
flipAsEither (Same x) = Left x
flipAsEither (Flip x) = Right x

flipWrap :: Flipping a -> b -> Flipping b
flipWrap (Same _) = Same
flipWrap (Flip _) = Flip

flipFlip :: (a -> (b, b) -> c) -> Flipping a -> (b, b) -> c
flipFlip f (Same x) = f x
flipFlip f (Flip x) = f x . swap

flipMap :: (a -> (b, b) -> c) -> Flipping a -> (b, b) -> Flipping c
flipMap f x = flipWrap x . flipFlip f x

flipExtractWith :: (a -> b, a -> b) -> Flipping a -> b
flipExtractWith (f, _) (Same x) = f x
flipExtractWith (_, f) (Flip x) = f x

instance Functor Flipping where
  fmap f (Same x) = Same (f x)
  fmap f (Flip x) = Flip (f x)

flipExtract :: Flipping a -> a
flipExtract (Same x) = x
flipExtract (Flip x) = x

flipInjectF :: Functor f => Flipping (f a) -> f (Flipping a)
flipInjectF x = fmap (flipWrap x) . flipExtract $ x
