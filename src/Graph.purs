module Graph where

import Prelude
import Effect.Console (log)

import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event as WE

import Circles as Circles

type WindowSize = { width :: Int, height :: Int }


type CircleId = String
type Edge = { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }
type Circle = { id :: String, pos :: Drag.PageCoord, text :: String }

type AppState = { windowSize :: WindowSize
                }

initialState :: WindowSize -> AppState
initialState = \windowSize -> {  windowSize : windowSize
                              }

data Query a =
    ResizeWindow { width :: Int, height :: Int } a
  | PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | HandleCircle Circles.Message a


data Message = Message Unit

type Input = WindowSize


--data Slot = CircleSlot UUID
data Slot = CircleSlot String
derive instance eqCircleSlot :: Eq Slot
derive instance ordCircleSlot :: Ord Slot

graph :: H.Component HH.HTML Query Input Message Aff
graph =
  H.parentComponent
    { initialState : initialState
    , render : render
    , eval : eval
    , receiver : HE.input ResizeWindow
    }
  where

  render :: AppState -> H.ParentHTML Query Circles.Query Slot Aff
  render state =
    HH.div_
    [ HH.slot (CircleSlot "asdf") Circles.circleNode state.windowSize (HE.input HandleCircle) ]

  eval :: Query ~> H.ParentDSL AppState Query Circles.Query Slot Message Aff
  eval = case _ of
    PreventDefault e q -> do
      H.liftEffect $ WE.preventDefault e
      eval q
    StopPropagation e q -> do
      H.liftEffect $ WE.stopPropagation e
      eval q
    ResizeWindow windowSize next -> next <$ do
      state <- H.get
      H.put $ state { windowSize = windowSize }
    HandleCircle (Circles.Drag id dragData) next -> next <$ do
      H.liftEffect $ log $ "Dragging circle " <> id <> " to " <> show dragData
      pure unit
    HandleCircle (Circles.Mouseover maybeId) next -> next <$ do
      case maybeId of
        Nothing -> pure unit
        Just id -> do
          H.liftEffect $ log $ "Mouseover circle " <> id
    HandleCircle (Circles.DragEnd id) next -> next <$ do
      H.liftEffect $ log $ "Dragend for circle " <> id
      pure unit
    HandleCircle (Circles.Delete id) next -> next <$ do
      H.liftEffect $ log $ "Delete circle " <> id
      _ <- H.query (CircleSlot "asdf") $ H.action $ Circles.TestLog "message from parent: delete received"
      pure unit
