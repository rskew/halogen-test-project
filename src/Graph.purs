module Graph where

import Prelude
import Effect.Console (log)

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
--import Data.Map (Map(..))
--import Data.Map as Map
--import Data.Tuple (Tuple(..))

import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
--import Halogen.HTML.Properties as HP
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

import Svg.Elements as SE
import Svg.Attributes as SA
--import Svg.Elements.Keyed as SK

import Data.UUID (UUID, genUUID)

import Circles as Circles

type WindowSize = { width :: Int, height :: Int }


type Edge = { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }

type AppState = { --circles :: Map Circles.CircleId Circles.CircleState
                  circle :: Circles.CircleState
                , edges :: Array Edge
                , drawingArrowState :: Maybe { x1 :: Number
                                             , y1 :: Number
                                             , x2 :: Number
                                             , y2 :: Number
                                             }
                , dragSource :: Maybe Circles.CircleId
                , dropTarget :: Maybe Circles.CircleId
                , windowSize :: WindowSize
                }

initialState :: { windowSize :: WindowSize, id :: UUID } -> AppState
initialState = \inputs -> { --circles : Map.fromFoldable
                                --[ Tuple "goober"
                                --  { id : "goober"
                                --  , pos : { pageX : 150.0
                                --          , pageY : 150.0
                                --          }
                                --  , text : "goober"
                                --  }
                                --, Tuple "asdf"
                                circle :
                                  { id : inputs.id
                                  , pos : { pageX : 750.0
                                          , pageY : 550.0
                                          }
                                  , text : "asdf\nfdsa"
                                  }
                                --]
                              , edges : []
                              , drawingArrowState : Nothing
                              , dragSource : Nothing
                              , dropTarget : Nothing
                              , windowSize : inputs.windowSize
                              }

data Query a =
    ResizeWindow { width :: Int, height :: Int } a
  | PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | HandleCircle Circles.Message a
  | CreateNode ME.MouseEvent a

data Message = Message Unit

type Input = { windowSize :: WindowSize, id :: UUID }


data Slot = CircleSlot UUID
derive instance eqCircleSlot :: Eq Slot
derive instance ordCircleSlot :: Ord Slot

graph :: H.Component HH.HTML Query Input Message Aff
graph =
  H.parentComponent
    { initialState : initialState
    , render : render
    , eval : eval
    , receiver : HE.input ResizeWindow <<< _.windowSize
    }
  where

  render :: AppState -> H.ParentHTML Query Circles.Query Slot Aff
  render state =
    let
      --lineView = \lineState -> SE.line
      --                         [ SA.x1  lineState.x1
      --                         , SA.y1  lineState.y1
      --                         , SA.x2  lineState.x2
      --                         , SA.y2  lineState.y2
      --                         ]
      --drawingArrowView = \maybeDrawingArrowState -> case maybeDrawingArrowState of
      --  Nothing -> []
      --  Just drawingArrowState -> [ SE.line
      --                              [ SA.x1 drawingArrowState.x1
      --                              , SA.y1 drawingArrowState.y1
      --                              , SA.x2 drawingArrowState.x2
      --                              , SA.y2 drawingArrowState.y2
      --                              , SA.markerEnd "url(#drawing-arrow)"
      --                              ]
      --                            ]
      --lines = lineView <$> state.edges
      --svgView = \keyedComponents -> SE.svg
      --                              [ SA.viewBox
      --                                0.0
      --                                0.0
      --                                (toNumber state.windowSize.width)
      --                                (toNumber state.windowSize.height)
      --                              , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e) $ H.action $ CreateNode e
      --                              ]
      --                              [ defs
      --                              , SK.g
      --                                []
      --                                keyedComponents
      --                              ]
      svgView = \components -> SE.svg
                               [ SA.viewBox
                                 0.0
                                 0.0
                                 (toNumber state.windowSize.width)
                                 (toNumber state.windowSize.height)
                               , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e) $ H.action $ CreateNode e
                               ]
                               components
      --defs = SE.defs
      --       [ SE.marker
      --         [ HP.id_ "drawing-arrow"
      --         , SA.markerWidth 1000.0
      --         , SA.markerHeight 1000.0
      --           -- TODO: typeify refX/refY/orient/markerUnits in the svg repo
      --         , SA.refX "0.0"
      --         , SA.refY "5.0"
      --         , SA.orient "auto"
      --         , SA.markerUnits "userSpaceOnUse"
      --         ]
      --         [ SE.path [ SA.d [ SA.Abs (SA.M 0.0 0.0)
      --                          , SA.Abs (SA.L 10.0 5.0)
      --                          , SA.Abs (SA.L 0.0 10.0)
      --                          , SA.Abs SA.Z
      --                          ]
      --                   ]
      --         ]
      --       ]
    in
    svgView [ HH.slot (CircleSlot state.circle.id) Circles.circleNode state.circle (HE.input HandleCircle) ]

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
      H.liftEffect $ log $ "Dragging circle " <> show id <> " to " <> show dragData
      state <- H.get
      case state.dragSource of
        Nothing -> pure unit
        Just source ->
          --case Map.lookup source state.circles of
          --  Nothing -> pure unit
          --  Just circle -> do
          --    H.modify_ _{ circles = Map.insert source (circle { pos = Drag.mouseEventToPageCoord mouseEvent }) state.circles }
          pure unit
      --H.modify_ _{ drawingArrowState = Just { x1 : dragData.x - dragData.offsetX
      --                                      , y1 : dragData.y - dragData.offsetY
      --                                      , x2 : dragData.x
      --                                      , y2 : dragData.y
      --                                      }
      --           }
          --state <- H.get
          --let dropTarget = state.dropTarget >>= \dropTargetId -> Map.lookup dropTargetId state.circles
          --case dropTarget of
          --  Nothing -> pure unit
          --  Just circle -> case state.drawingArrowState of
          --    Nothing -> pure unit
          --    Just drawingArrowState ->
          --      H.modify_ _{ edges = state.edges <> [ drawingArrowState ]
          --                 , drawingArrowState = Nothing
          --                 }
    HandleCircle (Circles.Mouseover maybeId) next -> next <$ do
      case maybeId of
        Nothing -> pure unit
        Just id -> do
          H.liftEffect $ log $ "Mouseover circle " <> show id
    HandleCircle (Circles.DragEnd id) next -> next <$ do
      H.liftEffect $ log $ "Dragend for circle " <> show id
      state <- H.get
      case state.dragSource of
        Nothing -> pure unit
        Just source ->
          --case Map.lookup source state.circles of
          --  Nothing -> pure unit
          --  Just circle -> do
          --    H.raise $ DragEnd circle.id
          pure unit
          --state <- H.get
          --let dropTarget = state.dropTarget >>= \dropTargetId -> Map.lookup dropTargetId state.circles
          --case dropTarget of
          --  Nothing -> pure unit
          --  Just circle -> case state.drawingArrowState of
          --    Nothing -> pure unit
          --    Just drawingArrowState ->
          --      H.modify_ _{ edges = state.edges <> [ drawingArrowState ]
          --                 , drawingArrowState = Nothing
          --                 }
    HandleCircle (Circles.Delete id) next -> next <$ do
      H.liftEffect $ log $ "Delete circle " <> show id
      state <- H.get
      _ <- H.query (CircleSlot state.circle.id) $ H.action $ Circles.TestLog "message from parent: delete received"
      pure unit
      --state <- H.get
      --H.modify_ _{ circles = Map.delete circleId state.circles }
      --H.liftEffect $ log $ "Removed circle with id " <> circleId
    CreateNode mouseEvent next -> next <$ do
      state <- H.get
      newCircleId <- H.liftEffect genUUID >>= pure <<< show
      let newCircle = { id : newCircleId, pos : Drag.mouseEventToPageCoord mouseEvent, text : "" }
      --H.modify_ _{ circles = Map.insert newCircleId newCircle state.circles }
      --H.liftEffect $ log $ "Inserted circle with id " <> newCircleId
      H.liftEffect $ log $ "One day I'll insert a circle with id " <> newCircleId
