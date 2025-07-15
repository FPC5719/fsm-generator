module FSM.FSM where

import Clash.Prelude
import FSM.Model
import FSM.TH

transitionFSM "small"
  [t| () |] [t| Int |] [t| Int |]
  () smallTransition

transitionFSM "test"
  [t| () |] [t| Int |] [t| Int |]
  () testTransition
