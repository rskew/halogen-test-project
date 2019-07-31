module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)

-- TODO: remove
import Data.UUID (genUUID)

import Graph as G

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  w <- H.liftEffect window
  windowWidth <- H.liftEffect $ innerWidth w
  windowHeight <- H.liftEffect $ innerHeight w
  H.liftEffect $ log $ "Window size: " <> show windowWidth <> " " <> show windowHeight
  id <- H.liftEffect genUUID
  let input = { windowSize : { width : windowWidth, height : windowHeight }, id : id }
  runUI G.graph input body
