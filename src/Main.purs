module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

import Data.Array as Array
import Data.Map as Map
import Data.Tuple (Tuple(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (innerWidth, innerHeight)

-- TODO: remove
import Data.UUID (genUUID, parseUUID)

import Graph as G

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  w <- H.liftEffect window
  windowWidth <- H.liftEffect $ innerWidth w
  windowHeight <- H.liftEffect $ innerHeight w
  H.liftEffect $ log $ "Window size: " <> show windowWidth <> " " <> show windowHeight
  id <- H.liftEffect genUUID
  let inputCirclesStr = [ { id : "4c735c62-b449-4626-8e8a-1ecd1a88fc2d"
                          , pos : { pageX : 150.0
                                  , pageY : 150.0
                                  }
                          , text : "goober"
                          , textFieldShape : G.defaultTextFieldShape
                          }
                        , { id : "350b250b-31cd-460f-9613-bfe824558ed0"
                          , pos : { pageX : 750.0
                                  , pageY : 550.0
                                  }
                          , text : "asdf\nfdsa"
                          , textFieldShape : G.defaultTextFieldShape
                          }
                        ]
  let inputCircles = Map.fromFoldable
                     $ Array.mapMaybe
                     (\circleStr -> do
                         uuid <- parseUUID circleStr.id
                         pure $ Tuple uuid circleStr { id = uuid })
                     inputCirclesStr
  let input =  { circles : inputCircles, windowSize : { width : windowWidth, height : windowHeight } }
  runUI G.graph input body
