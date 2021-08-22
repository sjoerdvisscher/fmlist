-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FMList
-- Copyright   :  (c) Sjoerd Visscher 2009
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  portable
--
-- FoldMap lists: lists represented by their 'foldMap' function.
--
-- Examples:
--
-- > -- A right-infinite list
-- > c = 1 `cons` c
--
-- > -- A left-infinite list
-- > d = d `snoc` 2
--
-- > -- A middle-infinite list ??
-- > e = c `append` d
--
-- > *> head e
-- > 1
-- > *> last e
-- > 2
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}


module Data.FMList (

    FMList(..)
  , transform

  -- * Construction
  , empty
  , singleton
  , cons
  , snoc
  , viewl
  , viewr
  , pair
  , append

  , fromList
  , fromFoldable

  -- * Basic functions
  , null
  , length
  , genericLength

  , head
  , tail
  , last
  , init
  , reverse

  -- * Folding
  , toList
  , flatten
  , foldMapA

  , filter
  , filterM
  , take
  , drop
  , takeWhile
  , dropWhile

  , zip
  , zipWith

  -- * Unfolding
  , iterate
  , repeat
  , cycle
  , unfold
  , unfoldr

  -- * Transforming
  , mapMaybe
  , wither
  ) where

import Prelude
  ( (.), ($), ($!), flip, const, error, otherwise
  , Either(..), either
  , Bool(..), (&&)
  , Ord(..), Num(..), Int
  , Show(..), String, (++)
  )
import qualified Prelude as P
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing)
import Data.Monoid (Monoid, mempty, mappend, Dual(..), First(..), Last(..), Sum(..))

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as S
#endif

import Data.Foldable (Foldable, foldMap, foldr, toList)
import Data.Traversable (Traversable, traverse)
import Control.Monad hiding (filterM)
import Control.Monad.Fail as MF
import Control.Applicative

-- | 'FMList' is a 'foldMap' function wrapped up in a newtype.
--
newtype FMList a = FM { unFM :: forall m . Monoid m => (a -> m) -> m }

infixr 6 <>

-- We define our own (<>) instead of using the one from Data.Semigroup
-- or Data.Monoid. This has two advantages:
--
-- 1. It avoids certain annoying compatibility issues in constraints.
-- 2. In the situation (sadly common in this context) where things
--    don't specialize and we actually pass a Monoid dictionary, it's
--    faster to extract mempty from that dictionary than to first
--    extract the Semigroup superclass dictionary and then extract its
--    (<>) method.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

-- | The function 'transform' transforms a list by changing
-- the map function that is passed to 'foldMap'.
--
-- It has the following property:
--
-- @transform a . transform b = transform (b . a)@
--
-- For example:
--
--  * @  m >>= g@
--
--  * @= flatten (fmap g m)@
--
--  * @= flatten . fmap g $ m@
--
--  * @= transform foldMap . transform (. g) $ m@
--
--  * @= transform ((. g) . foldMap) m@
--
--  * @= transform (\\f -> foldMap f . g) m@
--
transform :: (forall m. Monoid m => (a -> m) -> (b -> m)) -> FMList b -> FMList a
transform t (FM l) = FM (l . t)

-- shorthand constructors
nil          :: FMList a
nil          = FM mempty

one          :: a -> FMList a
one x        = FM ($ x)

(><)         :: FMList a -> FMList a -> FMList a
FM l >< FM r = FM (l `mappend` r)

-- exported constructors
singleton    :: a -> FMList a
singleton    = one

cons         :: a -> FMList a -> FMList a
cons x l     = one x >< l

snoc         :: FMList a -> a -> FMList a
snoc l x     = l >< one x

pair         :: a -> a -> FMList a
pair l r     = one l >< one r

append       :: FMList a -> FMList a -> FMList a
append       = (><)



fromList     :: [a] -> FMList a
fromList     = fromFoldable

fromFoldable :: Foldable f => f a -> FMList a
fromFoldable l = FM $ flip foldMap l


mhead        :: FMList a -> Maybe a
mhead l      = getFirst (unFM l (First . Just))

null         :: FMList a -> Bool
null         = isNothing . mhead

length       :: FMList a -> Int
length       = genericLength

genericLength :: Num b => FMList a -> b
genericLength l = getSum $ unFM l (const $ Sum 1)

infixl 5 :<
data ViewL a
  = EmptyL
  | a :< FMList a
  deriving (Show, Functor, Foldable, Traversable)

unviewl :: ViewL a -> FMList a
unviewl EmptyL = nil
unviewl (x :< xs) = cons x xs

#if MIN_VERSION_base(4,9,0)
instance Semigroup (ViewL a) where
  EmptyL <> v = v
  (x :< xs) <> v = x :< (xs >< unviewl v)
#endif

instance Monoid (ViewL a) where
  mempty     = EmptyL
#if MIN_VERSION_base(4,9,0)
  mappend    = (S.<>)
#else
  EmptyL `mappend` v = v
  (x :< xs) `mappend` v = x :< (xs >< unviewl v)
#endif

infixr 5 :>
data ViewR a
  = EmptyR
  | FMList a :> a
  deriving (Show, Functor, Foldable, Traversable)

unviewr :: ViewR a -> FMList a
unviewr EmptyR = nil
unviewr (xs :> x) = xs `snoc` x

#if MIN_VERSION_base(4,9,0)
instance Semigroup (ViewR a) where
  v <> EmptyR = v
  v <> (xs :> x) = (unviewr v >< xs) :> x
#endif

instance Monoid (ViewR a) where
  mempty     = EmptyR
#if MIN_VERSION_base(4,9,0)
  mappend    = (S.<>)
#else
  v `mappend` EmptyR = v
  v `mappend` (xs :> x) =(unviewr v >< xs) :> x
#endif

viewl :: FMList a -> ViewL a
viewl = foldMap (:< nil)

viewr :: FMList a -> ViewR a
viewr = foldMap (nil :>)

head         :: FMList a -> a
head l       = mhead l `fromMaybeOrError` "Data.FMList.head: empty list"

tail         :: FMList a -> FMList a
tail l       = case viewl l of
  EmptyL  -> error "Data.FMList.tail: empty list"
  _ :< l' -> l'

last         :: FMList a -> a
last l       = getLast (unFM l (Last . Just)) `fromMaybeOrError` "Data.FMList.last: empty list"

init         :: FMList a -> FMList a
init l       = case viewr l of
  EmptyR  -> error "Data.FMList.init: empty list"
  l' :> _ -> l'

reverse      :: FMList a -> FMList a
reverse l    = FM $ getDual . unFM l . (Dual .)

flatten      :: Foldable t => FMList (t a) -> FMList a
flatten      = transform foldMap

filter       :: (a -> Bool) -> FMList a -> FMList a
filter p     = transform (\f x -> if p x then f x else mempty)

mapMaybe     :: (a -> Maybe b) -> FMList a -> FMList b
mapMaybe p   = transform (\f x -> maybe mempty f (p x))

filterM      :: Applicative m => (a -> m Bool) -> FMList a -> m (FMList a)
filterM p l  = unWrapApp $ unFM l $ \a ->
  let
    go pr
      | pr = one a
      | otherwise = nil
  in WrapApp $ fmap go (p a)

wither      :: Applicative m => (a -> m (Maybe b)) -> FMList a -> m (FMList b)
wither p l  = unWrapApp $ unFM l $ \a -> WrapApp $ fmap (maybe nil one) (p a)

-- transform the foldMap to foldr with state.
transformCS  :: (forall m. Monoid m => (b -> m) -> a -> (m -> s -> m) -> s -> m) -> s -> FMList a -> FMList b
transformCS t s0 l = FM $ \f -> foldr (\e r -> t f e (\a -> mappend a . r)) mempty l s0

take         :: (Ord n, Num n) => n -> FMList a -> FMList a
take         = transformCS (\f e c i -> if i > 0 then c (f e) (i-1) else mempty)

takeWhile    :: (a -> Bool) -> FMList a -> FMList a
takeWhile p  = transformCS (\f e c _ -> if p e then c (f e) True else mempty) True

drop         :: (Ord n, Num n) => n -> FMList a -> FMList a
drop         = transformCS (\f e c i -> if i > 0 then c mempty (i-1) else c (f e) 0)

dropWhile    :: (a -> Bool) -> FMList a -> FMList a
dropWhile p  = transformCS (\f e c ok -> if ok && p e then c mempty True else c (f e) False) True

zipWith      :: (a -> b -> c) -> FMList a -> FMList b -> FMList c
zipWith t xs ys = fromList $ P.zipWith t (toList xs) (toList ys)

zip          :: FMList a -> FMList b -> FMList (a,b)
zip          = zipWith (,)


iterate      :: (a -> a) -> a -> FMList a
iterate f x  = x `cons` iterate f (f x)

-- | 'repeat' buids an infinite list of a single value.
-- While infinite, the result is still accessible from both the start and end.
repeat       :: a -> FMList a
repeat       = cycle . one

-- | 'cycle' repeats a list to create an infinite list.
-- It is also accessible from the end, where @last (cycle l)@ equals @last l@.
--
-- Caution: @cycle 'empty'@ is an infinite loop.
cycle        :: FMList a -> FMList a
cycle l      = l >< cycle l >< l

-- | 'unfoldr' builds an 'FMList' from a seed value from left to right.
-- The function takes the element and returns 'Nothing'
-- if it is done producing the list or returns 'Just' @(a,b)@, in which
-- case, @a@ is a appended to the result and @b@ is used as the next
-- seed value in a recursive call.
--
-- A simple use of 'unfoldr':
--
-- > *> unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- > fromList [10,9,8,7,6,5,4,3,2,1]
--
unfoldr      :: (b -> Maybe (a, b)) -> b -> FMList a
unfoldr g = go
  where
    go b = maybe nil (\(a, b') -> cons a (go b')) (g b)

-- | 'unfold' builds a list from a seed value.
-- The function takes the seed and returns an 'FMList' of values.
-- If the value is 'Right' @a@, then @a@ is appended to the result, and if the
-- value is 'Left' @b@, then @b@ is used as seed value in a recursive call.
--
-- A simple use of 'unfold' (simulating unfoldl):
--
-- > *> unfold (\b -> if b == 0 then empty else Left (b-1) `pair` Right b) 10
-- > fromList [1,2,3,4,5,6,7,8,9,10]
--
unfold       :: (b -> FMList (Either b a)) -> b -> FMList a
unfold g     = transform (\f -> either (foldMap f . unfold g) f) . g

-- | This is essentially the same as 'Data.Monoid.Ap'. We include it here
-- partly for compatibility with older base versions. But as discussed at the
-- local definition of '<>', it can be slightly better for performance to have
-- 'mappend' for this type defined in terms of 'mappend' for the underlying
-- 'Monoid' rather than 'S.<>' for the underlying 'Semigroup'.
newtype WrapApp f m = WrapApp { unWrapApp :: f m }

#if MIN_VERSION_base(4,9,0)
instance (Applicative f, Monoid m) => Semigroup (WrapApp f m) where
  WrapApp a <> WrapApp b = WrapApp $ liftA2 (<>) a b

instance (Applicative f, Monoid m) => Monoid (WrapApp f m) where
  mempty  = WrapApp $ pure mempty
  mappend = (S.<>)
#else
instance (Applicative f, Monoid m) => Monoid (WrapApp f m) where
  mempty                          = WrapApp $ pure mempty
  mappend (WrapApp a) (WrapApp b) = WrapApp $ liftA2 mappend a b
#endif

-- | Map each element of a structure to an action, evaluate these actions from left to right,
-- and concat the monoid results.
foldMapA
  :: ( Foldable t, Applicative f, Monoid m)
  => (a -> f m) -> t a -> f m
foldMapA f = unWrapApp . foldMap (WrapApp . f)


instance Functor FMList where
  fmap g     = transform (\f -> f . g)
  a <$ l     = transform (\f -> const (f a)) l

instance Foldable FMList where
  foldMap m f = unFM f m

instance Traversable FMList where
  traverse f = foldMapA (fmap one . f)

instance Monad FMList where
  return     = pure
  m >>= g    = transform (\f -> foldMap f . g) m
  (>>)       = (*>)

instance MF.MonadFail FMList where
  fail _ = nil

instance Applicative FMList where
  pure       = one
  gs <*> xs  = transform (\f g -> unFM xs (f . g)) gs
  as <*  bs  = transform (\f a -> unFM bs (const (f a))) as
  as  *> bs  = transform (\f   -> const (unFM bs f)) as
#if MIN_VERSION_base (4,10,0)
  liftA2 g xs ys = transform (\f x -> unFM ys (\y -> f (g x y))) xs
#endif

#if MIN_VERSION_base(4,9,0)
instance Semigroup (FMList a) where
  (<>) = (><)
#endif

instance Monoid (FMList a) where
  mempty     = nil
#if MIN_VERSION_base(4,9,0)
  mappend    = (S.<>)
#else
  mappend    = (><)
#endif

instance MonadPlus FMList where
  mzero      = nil
  mplus      = (><)

instance Alternative FMList where
  empty      = nil
  (<|>)      = (><)

instance Show a => Show (FMList a) where
  show l     = "fromList " ++ (show $! toList l)


fromMaybeOrError :: Maybe a -> String -> a
fromMaybeOrError ma e = fromMaybe (error e) ma
