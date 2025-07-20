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
varInit = Vars 2 0 0 True

primeFSM :: FSM Vars () (Maybe Value)
primeFSM =
  [ Until falsePred -- Infinite loop
    [ Node  [|| \_ -> (do { i %= (+ 1); j .= 2; f .= True; pure Nothing }) ||]
    , Until [|| \_ -> (do { v <- ask; pure (v ^. f || v ^. j == v ^. i)}) ||]
      [ Node  [|| \_ -> (do { use j >>= (k .=); pure Nothing }) ||]
      , Until [|| \_ -> (do { v <- ask; pure (v ^. k <= 0) }) ||]
        [ Node  [|| \_ -> (do { use j >>= (k %=). subtract; pure Nothing }) ||]
        ]
      , If    [|| \_ -> (do { v <- ask; pure (v ^. k == 0) }) ||]
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
    [ Node  [|| \_ -> (do { i .= 10; pure (Just 233) }) ||]
    , Until [|| \_ -> (do { v <- ask; pure (v ^. i == 0) }) ||]
      [ Node  [|| \_ -> (do { i %= (subtract 1); Just <$> use i } ) ||]]
    ]
  ]

