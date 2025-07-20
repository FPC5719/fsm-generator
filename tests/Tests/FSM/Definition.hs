module Tests.FSM.Definition where

import FSM.Model
import Clash.Prelude

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

type Value = Signed 16

data Vars = Vars
  { _i :: Value
  , _j :: Value
  , _k :: Value
  , _f :: Bool
  } deriving (Generic, Lift, BitPack, NFDataX)
makeLenses ''Vars

varInit :: Vars
varInit = Vars 1 0 0 True

primeFSM :: FSM Vars () (Maybe Value)
primeFSM =
  [ Until falsePred
    [ Node  [|| \_ -> (do { i += 1; j .= 1; f .= True; pure Nothing }) ||]
    , Until [|| \_ -> (do { v <- ask; pure (not (v ^. f) || v ^. j == v ^. i)}) ||]
      [ Node  [|| \_ -> (do { j += 1; (k .=) =<< use i; pure Nothing }) ||]
      , Until [|| \_ -> (do { (0 >=) <$> view k }) ||]
        [ Node  [|| \_ -> (do { (k -=) =<< use j; pure Nothing }) ||]
        ]
      , If    [|| \_ -> (do { (0 ==) <$> view k }) ||]
        [ Node  [|| \_ -> (do { f .= False; pure Nothing }) ||]
        ]
        []
      ]
    , If    [|| \_ -> (do { view f }) ||]
      [ Node  [|| \_ -> (do { Just <$> use i }) ||]
      ]
      []
    ]
  ]


miniFSM :: FSM Vars () (Maybe Value)
miniFSM =
  [ Until falsePred
    [ Node  [|| \_ -> (do { i .= 10; pure $ Just 233 }) ||]
    , Until [|| \_ -> (do { v <- ask; pure (v ^. i == 0) }) ||]
      [ Node  [|| \_ -> (do { ii <- use i; i .= ii - 1; pure $ Just ii } ) ||]
      , If    [|| \_ -> (do { (== 0) . (`mod` 3) <$> view i } ) ||]
        [ Node  [|| \_ -> (do { pure $ Just 666 } ) ||]
        ]
        []
      ]
    ]
  ]
