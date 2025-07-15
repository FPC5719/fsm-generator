{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FSM.TH where

import Prelude
import qualified Clash.Prelude as CP
import Language.Haskell.TH

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Vector as V
import FSM.Model

-- | Generate the logic of a singleton.
--
-- The result @ExpQ@ has type
-- @(r, BitVector n) -> i -> ((r, BitVector n), o)@.
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
  if V.length e /= n + 1
    then error "Impossible: Bad array length!"
    else pure ()
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
        go [ (idx, ei) | (idx, Just ei) <- V.toList $ V.indexed e ]
  let kexp = [| k |]
  [| \(r, bv) i ->
       let (o, r') = $vfnexp i r
           bv'     = $efnexp i r
       in if testBit bv $kexp
          then ((r', bv'), o)
          else unpack 0
   |]


-- | Convert a transition graph into actual logic.
--
-- Must manually pass through the quotation of types to help
-- with type inference.
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
              ) :: ($tr, $bvty) -> $ti -> (($tr, $bvty), $to)
           |]
        | idx <- [1 .. n]
        ]
  -- Exactly one vertex is enable
  let st =
        [| \(r, bv) i -> CP.unpack
          $(foldl1
             (\u v -> [| $u CP..|. $v |])
             ((\f -> [| CP.pack ($f (r, bv) i) |]) <$> exps)
           )
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
