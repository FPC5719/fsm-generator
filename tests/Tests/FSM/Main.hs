module Tests.FSM.Main where

import Clash.Prelude
import FSM.Model
import FSM.TH

import Tests.FSM.Definition

transitionFSM "test"
  [t| () |] [t| Int |] [t| Int |]
  () testTransition
