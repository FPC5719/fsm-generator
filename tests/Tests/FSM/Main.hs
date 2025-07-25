module Tests.FSM.Main where

import Clash.Prelude
import FSM.Model
import FSM.TH
import qualified Data.Vector as V

import Tests.FSM.Definition


transitionFSM "prime"
  [t| Vars |] [t| () |] [t| Maybe Value |]
  varInit (getTransition primeFSM)

transitionFSM "mini"
  [t| Vars |] [t| () |] [t| Maybe Value |]
  varInit (getTransition miniFSM)

testOutput :: [Maybe Value]
testOutput = sampleN @System 500 $ prime (pure ())

testDebug :: IO ()
testDebug = do
  let t = getTransition miniFSM
  print $ count t
  print $ V.length (vertex t)
  print $ V.length (edge t)
  V.forM_ (edge t) $ \ei -> do
    s <- V.forM ei $ \ej -> do
      case ej of
        Just _ -> pure 'X'
        Nothing -> pure '.'
    putStrLn $ V.toList s
