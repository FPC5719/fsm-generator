{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
This module contains two levels of abstraction:

[@FSM@]:
  Provides syntax structures for writing FSMs.
[@Transition@]:
  Is the inner transition graph derived from @FSM@,
  and is used for generating actual logic.
-}
module FSM.Model where

import Prelude
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Haskell.TH

-- | A singleton captures the behavior at a specific state.
type Singleton r i o = Code Q (i -> State  r o)
-- | A predicate determines whether to change state.
type Predicate r i   = Code Q (i -> Reader r Bool)

-- | A FSM is a sequence of @Step@s.
type FSM r i o = [Step r i o]

data Step r i o where
  Node  :: Singleton r i o -> Step r i o
  Until :: Predicate r i -> FSM r i o -> Step r i o
  If    :: Predicate r i -> FSM r i o -> FSM r i o -> Step r i o


data Transition r i o = Transition
  { count  :: Int
  , vertex :: Vector (Singleton r i o)
  , edge   :: Vector (Vector (Maybe (Predicate r i)))
  }

instance Semigroup (Transition r i o) where
  t1 <> t2 = Transition
    { count  = count  t1 +    count t2
    , vertex = vertex t1 V.++ vertex t2
    , edge   =
      let (corner1, border1, border2, bulk1) = divmat (edge t1)
          (corner2, border3, border4, bulk2) = divmat (edge t2)
          arrout  = V.cons corner1 border2
          arrin   = V.cons corner2 border3
          mult    = (<$> arrout) $ (<$> arrin) . liftA2 andPred
          (corner3, border5, border6, bulk3) = divmat mult
          dummy   = repmat (count t2, count t1) Nothing
      in concat9
         corner3 border1 border5
         border6 bulk1   bulk3
         border4 dummy   bulk2
    }

instance Monoid (Transition r i o) where
  mempty = Transition
    { count  = 0
    , vertex = V.empty
    , edge   = V.singleton . V.singleton $ Just truePred
    }
  mappend = (<>)


stepTransition :: Step r i o -> Transition r i o
stepTransition (Node s) = Transition
  { count  = 1
  , vertex = V.singleton s
  , edge   = V.fromListN 2
    [ V.fromListN 2 [Nothing      , Just truePred]
    , V.fromListN 2 [Just truePred, Nothing      ]
    ]
  }
stepTransition (Until p fsm) = Transition
  { count  = count  t
  , vertex = vertex t
  , edge   =
    let (corner1, border1, border2, bulk1) = divmat (edge t)
        pp      = Just p
        np      = Just (notPred p)
        arrout  = liftA2 andPred pp <$> border2
        arrin   = liftA2 andPred np <$> border1
        liftAlt f x y = x <|> y <|> liftA2 f x y
        bulk2   = V.zipWith
          (maybe id (const $ V.zipWith (liftAlt orPred) arrin))
          arrout bulk1
    in concat4
       corner1 border1
       arrout  bulk2
  }
  where
    t = getTransition fsm
stepTransition (If p fsm1 fsm2) = Transition
  { count  = count  t1 +    count  t2
  , vertex = vertex t1 V.++ vertex t2
  , edge   =
    let (corner1, border1, border2, bulk1) = divmat (edge t1)
        (corner2, border3, border4, bulk2) = divmat (edge t2)
        dummy1  = repmat (count t1, count t2) Nothing
        dummy2  = repmat (count t2, count t1) Nothing
        pp      = Just p
        arrin1  =
          (liftA2 andPred (            pp) <$> border1)
        arrin3  =
          (liftA2 andPred (notPred <$> pp) <$> border3)
        liftAlt f x y = x <|> y <|> liftA2 f x y
        corner  = liftAlt orPred
          (liftA2 andPred (            pp)  $  corner1)
          (liftA2 andPred (notPred <$> pp)  $  corner2)
    in concat9
       corner  arrin1  arrin3
       border2 bulk1   dummy1
       border4 dummy2  bulk2
  }
  where
    t1 = getTransition fsm1
    t2 = getTransition fsm2

getTransition :: FSM r i o -> Transition r i o
getTransition = mconcat . fmap stepTransition

-- * Other useful definitions

-- | True predicate.
truePred :: Predicate r i
truePred = [|| const $ pure True ||]

-- | False predicate.
falsePred :: Predicate r i
falsePred = [|| const $ pure False ||]

-- | Logic and.
andPred :: Predicate r i -> Predicate r i -> Predicate r i
andPred p q = [|| (liftA2 . liftA2 $ (&&)) $$p $$q ||]

-- | Logic or.
orPred :: Predicate r i -> Predicate r i -> Predicate r i
orPred p q = [|| (liftA2 . liftA2 $ (||)) $$p $$q ||]

-- | Logic not.
notPred :: Predicate r i -> Predicate r i
notPred p = [|| (fmap . fmap $ not) $$p ||]

-- * Matrix helpers

divmat
  :: Vector (Vector a)
  -> (a, Vector a, Vector a, Vector (Vector a))
divmat m =
  ( V.head . fmap V.head $ m
  , V.head . fmap V.tail $ m
  , V.tail . fmap V.head $ m
  , V.tail . fmap V.tail $ m
  )

repmat :: (Int, Int) -> a -> Vector (Vector a)
repmat (n, m) x = V.replicate n . V.replicate m $ x

concat3 :: Vector a -> Vector a -> Vector a -> Vector a
concat3 x y z = x V.++ y V.++ z

concat4
  ::        a ->         Vector a
  -> Vector a -> Vector (Vector a)
  -> Vector (Vector a)
concat4 m11 m12 m21 m22 = (V.++)
  (V.zipWith (V.++) (pure $ pure m11) (pure m12))
  (V.zipWith (V.++) (pure <$>    m21) (     m22))

concat9
  ::        a ->          Vector a   ->          Vector a
  -> Vector a -> (Vector (Vector a)) -> (Vector (Vector a))
  -> Vector a -> (Vector (Vector a)) -> (Vector (Vector a))
  -> Vector (Vector a)
concat9 m11 m12 m13 m21 m22 m23 m31 m32 m33 = concat3
  (V.zipWith3 concat3 (pure  $ pure m11) (pure m12) (pure m13))
  (V.zipWith3 concat3 (pure <$>     m21) (     m22) (     m23))
  (V.zipWith3 concat3 (pure <$>     m31) (     m32) (     m33))
