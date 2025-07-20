{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
This module provides Template Haskell functions to
generate actual logic code based on FSM desciptions.
-}

module FSM.TH where

import Prelude
import qualified Clash.Prelude as CP
import Language.Haskell.TH

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Vector as V
import FSM.Model

-- | Generate the logic of a singleton.
--
-- The result @`ExpQ`@ has type
-- @(r, `CP.BitVector` n) -> i -> Maybe ((r, `CP.BitVector` n), o)@.
-- However, since @n@ is not available at type checking,
-- typed quotation is not possible.
combineNode
  :: Int -- ^ @n@: Number of all vertices
  -> Int -- ^ @k@: Current vertex number (within 1 ~ n)
  -> Singleton r i o
  -> V.Vector (Maybe (Predicate r i))
  -- ^ Edges associated with current vertex
  -> ExpQ
combineNode n k v e = do
  let bvty = [t| CP.BitVector $(litT . numTyLit . toInteger $ n) |]
  let vfnexp :: ExpQ -- i -> r -> (o, r)
      vfnexp = [| \i r -> runState ($(unTypeCode v) i) r |]
  let efnexp :: ExpQ -- i -> r -> BitVector n
      efnexp = do
        let go :: [(Int, Predicate r i)] -> ExpQ
            go [] = [| \_ _ -> (0 :: $bvty) |]
            go ((l, p) : ps) =
              [| \i r -> if runReader ($(unTypeCode p) i) r
                   then setBit (0 :: $bvty) l
                   else $(go ps) i r
               |]
        go [ (idx, ei)
           | (idx, Just ei) <- V.toList . V.indexed . V.tail $ e
           ]
  let kexp = [| k |]
  [| \(r, bv) i ->
       let (o, r') = $vfnexp i r
           bv'     = $efnexp i r
       in if testBit bv ($kexp - 1)
          then Just ((r', bv'), o)
          else Nothing
   |]


-- | Convert a transition graph into actual logic.
--
-- Must manually pass through the quotation of types to help
-- with type inference.
--
-- Example:
--
-- Given @testFSM :: `FSM` r i o@ where @r@, @i@, @o@ are types
-- known at compile time, the following top-level slice:
--
-- @
-- `transitionFSM` "test"
--   [t| r |] [t| i |] [t| o |]
--   initialState (`getTransition` testFSM)
-- @
--
-- would generate a top-level definition @test@ with the type
-- @`CP.HiddenClockResetEnable` dom => `CP.Signal` dom i -> `CP.Signal` dom o@.
transitionFSM
  :: (CP.Lift r, CP.BitPack r, CP.BitPack o)
  => String
  -> TypeQ -- ^ Quotation of @r@
  -> TypeQ -- ^ Quotation of @i@
  -> TypeQ -- ^ Quotation of @o@
  -> r     -- ^ Initial state
  -> Transition r i o
  -> DecsQ
transitionFSM name tr ti to r0 t = do
  let n = count t
  let bvty = [t| CP.BitVector $(litT . numTyLit . toInteger $ n) |]
  -- Convert each vertex
  let exps =
        [ [| $(combineNode n idx
                (vertex t V.! (idx - 1)) (edge t V.! idx)
              ) :: ($tr, $bvty) -> $ti -> Maybe (($tr, $bvty), $to)
           |]
        | idx <- [1 .. n]
        ]
  -- Exactly one state is enabled
  let st =
        [| \(r, bv) i -> case
            $(foldl1
              (\u v -> [| $u <|> $v |])
              ((\f -> [| $f (r, bv) i |]) <$> exps)
             )
            of Just x -> x
         |]
  -- `foo` is a temporary name
  [sig, dec] <-
    [d| foo
          :: forall dom . CP.HiddenClockResetEnable dom
          => CP.Signal dom $ti -> CP.Signal dom $to
        foo = CP.mealy $st (r0, 1 :: $bvty)
      |]
  -- Rename
  let nm = mkName name
  let sig' = case sig of
        SigD _ ty -> SigD nm ty
        _ -> error "Impossible when generating signature!"
  let dec' = case dec of
        ValD (VarP _) body d -> ValD (VarP nm) body d
        _ -> error "Impossible when generating declaration!"
  pure [sig', dec']
