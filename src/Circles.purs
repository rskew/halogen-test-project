module Circles where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.KeyboardEvent as KE
import Web.DOM.Node as DN
import Web.DOM.NodeList as NL
import Web.Event.Event as WE

import Data.UUID (genUUID)

type WindowSize = { width :: Int, height :: Int }


type CircleId = String
type Edge = { x1 :: Number, y1 :: Number, x2 :: Number, y2 :: Number }
type Circle = { id :: String, pos :: Drag.PageCoord, text :: String }

type CircleState = { windowSize :: WindowSize
                   , dragState :: Maybe Drag.DragData
                   , circles :: Map CircleId Circle
                   , edges :: Array Edge
                   , drawingArrowState :: Maybe { x1 :: Number
                                                , y1 :: Number
                                                , x2 :: Number
                                                , y2 :: Number
                                                }
                   , dragSource :: Maybe CircleId
                   , dropTarget :: Maybe CircleId
                   }

initialState :: WindowSize -> CircleState
initialState = \windowSize -> { circles : Map.fromFoldable
                                [ Tuple "goober"
                                  { id : "goober"
                                  , pos : { pageX : 150.0
                                          , pageY : 150.0
                                          }
                                  , text : "goober"
                                  }
                                , Tuple "asdf"
                                  { id : "asdf"
                                  , pos : { pageX : 750.0
                                          , pageY : 550.0
                                          }
                                  , text : "asdf\nfdsa"
                                  }
                                ]
                              , edges : []
                              , windowSize : windowSize
                              , dragState : Nothing
                              , drawingArrowState : Nothing
                              , dragSource : Nothing
                              , dropTarget : Nothing
                              }

data Query a =
    ResizeWindow { width :: Int, height :: Int } a
  | DragStart CircleId ME.MouseEvent a
  | DragMove Drag.DragEvent a
  | DropTarget (Maybe CircleId) a
  | TextInput CircleId KE.KeyboardEvent a
  | CreateNode ME.MouseEvent a
  | DeleteNode CircleId a
  | PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | TestLog String a


data Message =
    Drag CircleId Drag.DragData
  | Mouseover (Maybe CircleId)
  | DragEnd CircleId
  | Delete CircleId

circleNode :: H.Component HH.HTML Query { width :: Int, height :: Int } Message Aff
circleNode =
  H.lifecycleComponent
    { initialState : initialState
    , render : render
    , eval : eval
    , receiver : HE.input ResizeWindow
    , initializer : Nothing
    , finalizer : Nothing
    }
  where

  render :: CircleState -> H.ComponentHTML Query
  render state =
    let
      circleView = \circleState -> SE.g []
                                   [ SE.circle
                                     [ HP.id_ circleState.id
                                     , SA.r 100.0
                                     , SA.cx circleState.pos.pageX
                                     , SA.cy circleState.pos.pageY
                                     , HE.onMouseDown $ HE.input $ DragStart circleState.id
                                     , HE.onMouseOver $ HE.input_ $ DropTarget $ Just circleState.id
                                     , HE.onMouseOut $ HE.input_ $ DropTarget $ Nothing
                                     , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e) $ H.action $ DeleteNode circleState.id
                                     ]
                                   , SE.foreignObject
                                     [ SA.x $ circleState.pos.pageX + 120.0
                                     , SA.y $ circleState.pos.pageY
                                       -- TODO: size this properly (possibly reponsively to the scrollWidth of the div)
                                     , SA.height 400.0
                                     , SA.width 400.0
                                     , HE.onKeyUp $ HE.input $ TextInput circleState.id
                                     ]
                                     [ HH.div
                                       [ HH.attr (HH.AttrName "xmlns") "http://www.w3.org/1999/xhtml"
                                       , HP.class_ $ HH.ClassName "text-field"
                                       , HH.attr (HH.AttrName "contenteditable") "true"
                                       ]
                                       []
                                     ]
                                   ]
      lineView = \lineState -> SE.line
                               [ SA.x1  lineState.x1
                               , SA.y1  lineState.y1
                               , SA.x2  lineState.x2
                               , SA.y2  lineState.y2
                               ]
      drawingArrowView = \maybeDrawingArrowState -> case maybeDrawingArrowState of
        Nothing -> []
        Just drawingArrowState -> [ SE.line
                                    [ SA.x1 drawingArrowState.x1
                                    , SA.y1 drawingArrowState.y1
                                    , SA.x2 drawingArrowState.x2
                                    , SA.y2 drawingArrowState.y2
                                    , SA.markerEnd "url(#drawing-arrow)"
                                    ]
                                  ]
      lines = lineView <$> state.edges
      circles = Map.toUnfoldable $ Map.mapMaybe (circleView >>> Just) state.circles
      svgView = \keyedComponents -> SE.svg
                               [ SA.viewBox
                                 0.0
                                 0.0
                                 (toNumber state.windowSize.width)
                                 (toNumber state.windowSize.height)
                               , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e) $ H.action $ CreateNode e
                               ]
                               [ defs
                               , SK.g
                                 []
                                 keyedComponents
                               ]
      defs = SE.defs
             [ SE.marker
               [ HP.id_ "drawing-arrow"
               , SA.markerWidth 1000.0
               , SA.markerHeight 1000.0
                 -- TODO: typeify refX/refY/orient/markerUnits in the svg repo
               , SA.refX "0.0"
               , SA.refY "5.0"
               , SA.orient "auto"
               , SA.markerUnits "userSpaceOnUse"
               ]
               [ SE.path [ SA.d [ SA.Abs (SA.M 0.0 0.0)
                                , SA.Abs (SA.L 10.0 5.0)
                                , SA.Abs (SA.L 0.0 10.0)
                                , SA.Abs SA.Z
                                ]
                         ]
               ]
             ]
      in
        HH.div_
        --[ svgView (drawingArrowView state.drawingArrowState <> circles <> lines) ]
        [ svgView circles ]

  eval :: Query ~> H.ComponentDSL CircleState Query Message Aff
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
    DragStart circleId mouseEvent next -> next <$ do
      H.subscribe $ Drag.dragEventSource mouseEvent $ \e -> Just $ DragMove e H.Listening
      H.modify_ _{ dragSource = Just circleId }
    DragMove dragEvent next -> next <$ do
      state <- H.get
      case dragEvent of
        Drag.Move mouseEvent dragData -> do
          case state.dragSource of
            Nothing -> pure unit
            Just source ->
              case Map.lookup source state.circles of
                Nothing -> pure unit
                Just circle -> do
                  H.modify_ _{ circles = Map.insert source (circle { pos = Drag.mouseEventToPageCoord mouseEvent }) state.circles }
                  H.raise $ Drag circle.id dragData
          --H.modify_ _{ drawingArrowState = Just { x1 : dragData.x - dragData.offsetX
          --                                      , y1 : dragData.y - dragData.offsetY
          --                                      , x2 : dragData.x
          --                                      , y2 : dragData.y
          --                                      }
          --           }
        Drag.Done mouseEvent -> do
          case state.dragSource of
            Nothing -> pure unit
            Just source ->
              case Map.lookup source state.circles of
                Nothing -> pure unit
                Just circle -> do
                  H.raise $ DragEnd circle.id
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
    DropTarget maybeTarget next -> next <$ do
      H.raise $ Mouseover maybeTarget
      H.modify_ _{ dropTarget = maybeTarget }
    TextInput graphNodeId keyboardEvent next -> next <$ do
      let event = KE.toEvent keyboardEvent
      let maybeNode = WE.target event >>= DN.fromEventTarget
      case maybeNode of
        Nothing -> pure unit
        Just node -> do
          nodeText <- H.liftEffect $ getContentEditableText node
          -- TODO: lenses
          state <- H.get
          let thisCircle = Map.lookup graphNodeId state.circles
          case thisCircle of
            Nothing -> pure unit
            Just circle -> do
              H.modify_ _{ circles = Map.insert graphNodeId (circle { text = nodeText }) state.circles }
    CreateNode mouseEvent next -> next <$ do
      state <- H.get
      newCircleId <- H.liftEffect genUUID >>= pure <<< show
      let newCircle = { id : newCircleId, pos : Drag.mouseEventToPageCoord mouseEvent, text : "" }
      H.modify_ _{ circles = Map.insert newCircleId newCircle state.circles }
      H.liftEffect $ log $ "Inserted circle with id " <> newCircleId
    DeleteNode circleId next -> next <$ do
      state <- H.get
      H.modify_ _{ circles = Map.delete circleId state.circles }
      H.liftEffect $ log $ "Removed circle with id " <> circleId
      H.raise $ Delete circleId
    TestLog str next -> next <$ do
      H.liftEffect $ log str

getContentEditableText :: DN.Node -> Effect String
getContentEditableText node = do
  childrenNodeList <- DN.childNodes node
  children <- NL.toArray childrenNodeList
  nodeText <- traverse DN.textContent children
  pure $ String.joinWith "\n" nodeText
