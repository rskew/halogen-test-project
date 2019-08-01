module Graph where

import Prelude

import ContentEditable as ContentEditable
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Ord as Ord
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

defaultTextFieldShape :: { width :: Number, height :: Number }
defaultTextFieldShape = { width : 100.0, height : 50.0 }

maxTextFieldShape :: { width :: Number, height :: Number }
maxTextFieldShape = { width : 700.0, height : 500.0 }

type WindowSize = { width :: Int, height :: Int }

type CircleId = UUID
type CircleState = { id :: UUID, pos :: Drag.PageCoord, text :: String , textFieldShape :: { height :: Number, width :: Number} }

type Edge = { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }

type AppState = { circles :: Map UUID CircleState
                , edges :: Array Edge
                , drawingArrowState :: Maybe { x1 :: Number
                                             , y1 :: Number
                                             , x2 :: Number
                                             , y2 :: Number
                                             }
                , dragSource :: Maybe CircleId
                , dropTarget :: Maybe CircleId
                , windowSize :: WindowSize
                }

initialState :: { windowSize :: WindowSize, circles :: Map UUID CircleState } -> AppState
initialState inputs =
  { circles : inputs.circles
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
  | CreateNode ME.MouseEvent a
  | DragStart CircleId ME.MouseEvent a
  | DragMove CircleId Drag.DragEvent a
  | DropTarget (Maybe CircleId) a
  | HandleTextInput CircleId ContentEditable.Message a
  | DeleteNode CircleId a

data Message = Message Unit

type Input = { windowSize :: WindowSize, circles :: Map UUID CircleState }

data Slot = TextField UUID
derive instance eqTextFieldSlot :: Eq Slot
derive instance ordTextFieldSlot :: Ord Slot

graph :: H.Component HH.HTML Query Input Message Aff
graph =
  H.parentComponent
    { initialState : initialState
    , render : render
    , eval : eval
    , receiver : HE.input ResizeWindow <<< _.windowSize
    }
  where

  renderGraphNode :: CircleState -> H.ParentHTML Query ContentEditable.Query Slot Aff
  renderGraphNode state =
    SE.g []
    [ SE.circle
      [ HP.id_ $ show state.id
      , SA.r 100.0
      , SA.cx state.pos.pageX
      , SA.cy state.pos.pageY
      , HE.onMouseDown $ HE.input $ DragStart state.id
      , HE.onMouseOver $ HE.input_ $ DropTarget $ Just state.id
      , HE.onMouseOut $ HE.input_ $ DropTarget $ Nothing
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e) $ H.action $ DeleteNode state.id
      ]
    , SE.foreignObject
      [ SA.x $ state.pos.pageX + 120.0
      , SA.y $ state.pos.pageY
        -- TODO: size this properly (possibly reponsively to the scrollWidth of the div)
      , SA.height state.textFieldShape.height
      , SA.width state.textFieldShape.width
      ]
      [ HH.slot (TextField state.id) ContentEditable.contenteditable state.text (HE.input (HandleTextInput state.id)) ]
    ]

  render :: AppState -> H.ParentHTML Query ContentEditable.Query Slot Aff
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
      keyedCircles = (\circle ->
        Tuple (show circle.id) $ renderGraphNode circle
        ) <$> (Array.fromFoldable $ Map.values state.circles)
    in
      SE.svg
      [ SA.viewBox
        0.0
        0.0
        (toNumber state.windowSize.width)
        (toNumber state.windowSize.height)
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e) $ H.action $ CreateNode e
      ]
      [ SK.g
        []
        keyedCircles
      ]

  eval :: Query ~> H.ParentDSL AppState Query ContentEditable.Query Slot Message Aff
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
    HandleTextInput id (ContentEditable.TextUpdate text) next -> next <$ do
      state <- H.get
      case Map.lookup id state.circles of
        Nothing -> pure unit
        Just circle -> do
          maybeMaybeTextFieldScrollShape <- H.query (TextField id) $ H.request ContentEditable.GetScrollShape
          let textFieldScrollShape =
                fromMaybe defaultTextFieldShape
                $ fromMaybe Nothing maybeMaybeTextFieldScrollShape
          let clippedScrollShape = { width : Ord.min textFieldScrollShape.width maxTextFieldShape.width
                                   , height : Ord.min textFieldScrollShape.height maxTextFieldShape.height
                                   }
          H.modify_ _{ circles = Map.insert id (circle { text = text
                                                       , textFieldShape = clippedScrollShape
                                                       }) state.circles }
    DragStart id mouseEvent next -> next <$ do
      H.subscribe $ Drag.dragEventSource mouseEvent $ \e -> Just $ DragMove id e H.Listening
    DragMove id dragEvent next -> next <$ do
      state <- H.get
      case dragEvent of
        Drag.Move mouseEvent dragData -> do
          case Map.lookup id state.circles of
            Nothing -> pure unit
            Just circle -> do
              H.modify_ _{ circles = Map.insert id (circle { pos = Drag.mouseEventToPageCoord mouseEvent }) state.circles }
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
        Drag.Done mouseEvent -> do
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
          H.modify_ _{ dragSource = Nothing }

    DropTarget maybeTarget next -> next <$ do
      H.modify_ _{ dropTarget = maybeTarget }
    DeleteNode id next -> next <$ do
      state <- H.get
      case Map.lookup id state.circles of
        Nothing -> pure unit
        Just circle -> do
          H.liftEffect $ log circle.text
          H.liftEffect $ log $ show id
      H.modify_ _{ circles = Map.delete id state.circles }
      pure unit
    CreateNode mouseEvent next -> next <$ do
      newCircleId <- H.liftEffect genUUID
      let newCircle = { id : newCircleId, pos : Drag.mouseEventToPageCoord mouseEvent, text : "newnode", textFieldShape : defaultTextFieldShape }
      state <- H.get
      H.modify_ _{ circles = Map.insert newCircleId newCircle state.circles }
