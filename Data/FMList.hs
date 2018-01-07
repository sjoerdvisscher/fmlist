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


module Data.FMList (

    FMList(..)
  , transform

  -- * Construction
  , empty
  , singleton
  , cons
  , snoc
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

  ) where

import Prelude
  ( (.), ($), ($!), flip, const, error
  , Either(..), either
  , Bool(..), (&&)
  , Ord(..), Num(..), Int
  , Show(..), String, (++)
  )
import Data.Maybe (Maybe(..), maybe, fromMaybe, isNothing)
import Data.Monoid (Monoid, mempty, mappend, Dual(..), First(..), Last(..), Sum(..))

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup((<>)))
#endif

import Data.Foldable (Foldable, foldMap, foldr, toList)
import Data.Traversable (Traversable, traverse)
import Control.Monad
import Control.Applicative

-- | 'FMList' is a 'foldMap' function wrapped up in a newtype.
--
newtype FMList a = FM { unFM :: forall m . Monoid m => (a -> m) -> m }

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


head         :: FMList a -> a
head l       = mhead l `fromMaybeOrError` "Data.FMList.head: empty list"

tail         :: FMList a -> FMList a
tail l       = if null l then error "Data.FMList.tail: empty list" else drop (1::Int) l

last         :: FMList a -> a
last l       = getLast (unFM l (Last . Just)) `fromMaybeOrError` "Data.FMList.last: empty list"

init         :: FMList a -> FMList a
init l       = if null l then error "Data.FMList.init: empty list" else reverse . drop (1::Int) . reverse $ l

reverse      :: FMList a -> FMList a
reverse l    = FM $ getDual . unFM l . (Dual .)

flatten      :: Foldable t => FMList (t a) -> FMList a
flatten      = transform foldMap

filter       :: (a -> Bool) -> FMList a -> FMList a
filter p     = transform (\f x -> if p x then f x else mempty)


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
zipWith t    = transformCS (\f e2 c r1 -> foldr (\e1 _ -> c (f (t e1 e2)) (drop (1::Int) r1)) mempty r1)

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
unfoldr g    = unfold (maybe empty (\(a, b) -> Right a `pair` Left b) . g)

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


newtype WrapApp f m = WrapApp { unWrapApp :: f m }

#if MIN_VERSION_base(4,9,0)
instance (Applicative f, Semigroup m) => Semigroup (WrapApp f m) where
  WrapApp a <> WrapApp b = WrapApp $ (<>) <$> a <*> b
#endif

instance (Applicative f, Monoid m) => Monoid (WrapApp f m) where
  mempty                          = WrapApp $ pure mempty
  mappend (WrapApp a) (WrapApp b) = WrapApp $ mappend <$> a <*> b

-- | Map each element of a structure to an action, evaluate these actions from left to right,
-- and concat the monoid results.
foldMapA :: (Foldable t, Applicative f, Monoid m) => (a -> f m) -> t a -> f m
foldMapA f = unWrapApp . foldMap (WrapApp . f)



instance Functor FMList where
  fmap g     = transform (\f -> f . g)
  a <$ l     = transform (\f -> const (f a)) l

instance Foldable FMList where
  foldMap    = flip unFM

instance Traversable FMList where
  traverse f = foldMapA (fmap one . f)

instance Monad FMList where
  return     = one
  m >>= g    = transform (\f -> foldMap f . g) m
  m >> k     = transform (\f -> const (foldMap f k)) m
  fail _     = nil

instance Applicative FMList where
  pure       = one
  gs <*> xs  = transform (\f g -> unFM xs (f . g)) gs
  as <*  bs  = transform (\f a -> unFM bs (const (f a))) as
  as  *> bs  = transform (\f   -> const (unFM bs f)) as

#if MIN_VERSION_base(4,9,0)
instance Semigroup (FMList a) where
  (<>) = (><)
#endif

instance Monoid (FMList a) where
  mempty     = nil
  mappend    = (><)

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
