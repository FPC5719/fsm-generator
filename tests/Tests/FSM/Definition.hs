module Tests.FSM.Definition where

import FSM.Model
import FSM.TH
import Clash.Prelude

testFSM :: FSM () Int Int
testFSM =
  [ Until truePred
    [ Node $ [|| \_ -> pure 1 ||] ]
  , Node $ [|| \_ -> pure 2 ||]
  , If truePred
    [ Node $ [|| \_ -> pure 3 ||]
    , Node $ [|| \_ -> pure 4 ||] ]
    []
  , Node $ [|| \_ -> pure 5 ||]
  ]

testTransition :: Transition () Int Int
testTransition = getTransition testFSM
