module Graph where

import Prelude

import ContentEditable as ContentEditable
import Data.Array as Array
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord as Ord
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Svg.Attributes as SA
import Svg.Elements as SE
import Svg.Elements.Keyed as SK
import Web.Event.Event as WE
import Web.UIEvent.MouseEvent as ME

import Data.Lens (Lens', Traversal', lens, set, setJust, traversed, preview, filtered, firstOf)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

import Unsafe.Coerce (unsafeCoerce)

import Math as Math

nodeRadius :: Number
nodeRadius = 30.0

haloRadius :: Number
haloRadius = 60.0

type Shape = { width :: Number, height :: Number }

defaultTextFieldShape :: Shape
defaultTextFieldShape = { width : 100.0, height : 50.0 }

maxTextFieldShape :: Shape
maxTextFieldShape = { width : 700.0, height : 500.0 }

newtype GraphPosition = GraphPosition { x :: Number, y :: Number }

newtype PagePosition = PagePosition { x :: Number, y :: Number }

toGraphPosition :: PagePosition -> AppState -> GraphPosition
toGraphPosition (PagePosition pagePos) (AppState state) =
  let (PagePosition graphOrigin) = state.graphOrigin in
  GraphPosition $ pagePos - graphOrigin

toPagePosition :: GraphPosition -> AppState -> PagePosition
toPagePosition (GraphPosition graphPos) (AppState state) =
  let (PagePosition graphOrigin) = state.graphOrigin in
  PagePosition $ graphPos + graphOrigin

type CircleState = { id :: UUID, pos :: GraphPosition, text :: String , textFieldShape :: Shape }

type Edge = { source :: UUID
            , target :: UUID
            }

edgeId :: Edge -> String
edgeId edge = show edge.source <> "_" <> show edge.target

type DrawingEdge = { source :: UUID
                   , pos :: GraphPosition
                   }

drawingEdgeId :: UUID -> String
drawingEdgeId source = ("drawingEdge_" <> show source)

data DragSource =
    NodeDrag
  | HaloDrag
  | BackgroundDrag
derive instance eqDragSource :: Eq DragSource
derive instance ordDragSource :: Ord DragSource

newtype PositionLens = PositionLens (Traversal' AppState GraphPosition)

type DragContext = { initialPos :: GraphPosition
                   , sourceType :: DragSource
                   , elementId :: UUID
                   , elementPosLens :: PositionLens
                   }

newtype AppState = AppState
                   { circles :: Map UUID CircleState
                   , edges :: Set Edge
                   , drawingEdgeState :: Maybe DrawingEdge
                   , dropTarget :: Maybe UUID
                   , windowSize :: Shape
                   , graphOrigin :: PagePosition
                   , graphId :: UUID
                   }

_AppState :: Lens' AppState { circles :: Map UUID CircleState
                            , edges :: Set Edge
                            , drawingEdgeState :: Maybe DrawingEdge
                            , dropTarget :: Maybe UUID
                            , windowSize :: Shape
                            , graphOrigin :: PagePosition
                            , graphId :: UUID
                            }
_AppState = lens (\(AppState appState) -> appState) (\_ -> AppState)

_drawingEdgeState :: Lens' AppState (Maybe DrawingEdge)
_drawingEdgeState = _AppState <<< prop (SProxy :: SProxy "drawingEdgeState")

_circles :: Lens' AppState (Map UUID CircleState)
_circles = _AppState <<< prop (SProxy :: SProxy "circles")

_circlePos :: Lens' CircleState GraphPosition
_circlePos = prop (SProxy :: SProxy "pos")

_drawingEdgePos :: Traversal' AppState GraphPosition
_drawingEdgePos = _drawingEdgeState <<< traversed <<< prop (SProxy :: SProxy "pos")

_nodePos :: UUID -> Traversal' AppState GraphPosition
_nodePos id = _circles <<< at id <<< traversed <<< _circlePos

_graphOrigin :: Lens' AppState PagePosition
_graphOrigin = _AppState <<< prop (SProxy :: SProxy "graphOrigin")

_graphOriginAsGraphPosition :: Lens' AppState GraphPosition
_graphOriginAsGraphPosition = lens
                              (\(AppState state) -> unsafeCoerce state.graphOrigin)
                              (\(AppState state) newGraphPos ->
                                AppState $ state { graphOrigin = unsafeCoerce newGraphPos })

_edges :: Lens' AppState (Set Edge)
_edges = _AppState <<< prop (SProxy :: SProxy "edges")

initialState :: { graphId :: UUID, windowSize :: Shape, circles :: Map UUID CircleState } -> AppState
initialState inputs = AppState
  { circles : inputs.circles
  , edges : Set.empty
  , drawingEdgeState : Nothing
  , dropTarget : Nothing
  , windowSize : inputs.windowSize
  , graphOrigin : PagePosition { x : 0.0, y : 0.0 }
  , graphId : inputs.graphId
  }

data Query a =
    PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | ResizeWindow Shape a
  | DragStart DragContext ME.MouseEvent a
  | DragMove DragContext Drag.DragEvent a
  | BeginEdgeDraw DragContext ME.MouseEvent (DragContext -> Query a)
  | EndEdgeDraw a
  | HandleTextInput UUID ContentEditable.Message a
  | CreateNode ME.MouseEvent a
  | DeleteNode UUID a

data Message = Message Unit

type Input = { graphId :: UUID, windowSize :: Shape, circles :: Map UUID CircleState }

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

  renderGraphNode :: CircleState -> AppState -> H.ParentHTML Query ContentEditable.Query Slot Aff
  renderGraphNode circleState (AppState state) =
    let
      PagePosition nodePagePos = toPagePosition circleState.pos (AppState state)
      haloDragContext = { initialPos : circleState.pos
                        , elementId : circleState.id
                        , sourceType : HaloDrag
                        , elementPosLens : PositionLens _drawingEdgePos
                        }
      nodeDragContext = { initialPos : circleState.pos
                        , elementId : circleState.id
                        , sourceType : NodeDrag
                        , elementPosLens : PositionLens (_nodePos circleState.id)
                        }
    in
      SE.g []
      [ SE.circle
        [ SA.class_ "halo"
        , SA.r haloRadius
        , SA.cx nodePagePos.x
        , SA.cy nodePagePos.y
        , HE.onMouseDown \e -> Just $ StopPropagation (ME.toEvent e)
                               $ BeginEdgeDraw haloDragContext e
                               $ \drawEdgeContext -> H.action $ DragStart drawEdgeContext e
        ]
      , SE.circle
        [ SA.class_ "node"
        , SA.r nodeRadius
        , SA.cx nodePagePos.x
        , SA.cy nodePagePos.y
        , HE.onMouseDown \e -> Just
                               $ StopPropagation (ME.toEvent e)
                               $ H.action $ DragStart nodeDragContext e
        , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e) $ H.action $ DeleteNode circleState.id
        ]
      , SE.foreignObject
        [ SA.x $ nodePagePos.x + 90.0
        , SA.y $ nodePagePos.y
        , SA.height circleState.textFieldShape.height
        , SA.width circleState.textFieldShape.width
        ]
        [ HH.slot
          (TextField circleState.id)
          ContentEditable.contenteditable
          circleState.text
          (HE.input (HandleTextInput circleState.id))
        ]
      ]

  svgDefs :: H.ParentHTML Query ContentEditable.Query Slot Aff
  svgDefs = SE.defs
            [ SE.marker
              [ SA.id "drawing-arrow"
              , SA.markerWidth 10.0
              , SA.markerHeight 10.0
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

  renderEdge :: AppState -> Edge -> Maybe (H.ParentHTML Query ContentEditable.Query Slot Aff)
  renderEdge (AppState state) edge = do
    sourceNode <- Map.lookup edge.source state.circles
    targetNode <- Map.lookup edge.target state.circles
    let
      PagePosition sourcePagePos = toPagePosition sourceNode.pos (AppState state)
      PagePosition targetPagePos = toPagePosition targetNode.pos (AppState state)
    pure $
      SE.line
      [ SA.x1 sourcePagePos.x
      , SA.y1 sourcePagePos.y
      , SA.x2 targetPagePos.x
      , SA.y2 targetPagePos.y
      ]

  renderDrawingEdge :: GraphPosition -> GraphPosition -> AppState -> H.ParentHTML Query ContentEditable.Query Slot Aff
  renderDrawingEdge sourceGraphPos targetGraphPos (AppState state) =
    let
      PagePosition sourcePagePos = toPagePosition sourceGraphPos (AppState state)
      PagePosition targetPagePos = toPagePosition targetGraphPos (AppState state)
    in
      SE.line
      [ SA.x1 sourcePagePos.x
      , SA.y1 sourcePagePos.y
      , SA.x2 targetPagePos.x
      , SA.y2 targetPagePos.y
      , SA.markerEnd "url(#drawing-arrow)"
      ]

  render :: AppState -> H.ParentHTML Query ContentEditable.Query Slot Aff
  render (AppState state) =
    let
      keyedNodes = (\circle ->
                     Tuple (show circle.id) $ renderGraphNode circle (AppState state)
                   ) <$> (Array.fromFoldable $ Map.values state.circles)
      keyedEdges = Array.mapMaybe
                   (\edge -> do
                       edgeHTML <- renderEdge (AppState state) edge
                       pure $ Tuple (edgeId edge) edgeHTML
                   )
                   $ Set.toUnfoldable state.edges
      drawingEdges = fromMaybe [] $ do
        drawingEdgeState <- state.drawingEdgeState
        sourcePos <- preview (_nodePos drawingEdgeState.source) (AppState state)
        pure [ Tuple (drawingEdgeId drawingEdgeState.source) (renderDrawingEdge sourcePos drawingEdgeState.pos (AppState state)) ]
      backgroundDragContext = { initialPos : unsafeCoerce state.graphOrigin
                              , elementId : state.graphId
                              , sourceType : BackgroundDrag
                              , elementPosLens : PositionLens _graphOriginAsGraphPosition
                              }
    in
      SE.svg
      [ SA.viewBox
        0.0
        0.0
        state.windowSize.width
        state.windowSize.height
      , HE.onMouseDown $ HE.input $ DragStart backgroundDragContext
      , HE.onDoubleClick \e -> Just $ StopPropagation (ME.toEvent e) $ H.action $ CreateNode e
      ]
      [ svgDefs
      , SK.g
        []
        (keyedNodes <> keyedEdges <> drawingEdges )
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
      AppState state <- H.get
      H.put $ AppState $ state { windowSize = windowSize }

    HandleTextInput id (ContentEditable.TextUpdate text) next -> next <$ do
      AppState state <- H.get
      case Map.lookup id state.circles of
        Nothing -> pure unit
        Just circle -> do
          -- Update foreignObject shape to fit content
          maybeMaybeTextFieldScrollShape <- H.query (TextField id) $ H.request ContentEditable.GetScrollShape
          let textFieldScrollShape =
                fromMaybe defaultTextFieldShape
                $ fromMaybe Nothing maybeMaybeTextFieldScrollShape
          let clippedScrollShape = { width : Ord.min textFieldScrollShape.width maxTextFieldShape.width
                                   , height : Ord.min textFieldScrollShape.height maxTextFieldShape.height
                                   }
          let newCircle = circle { text = text
                                 , textFieldShape = clippedScrollShape
                                 }
          H.modify_ $ setJust (_circles <<< at id) newCircle

    DragStart dragContext mouseEvent next -> next <$ do
      H.subscribe $ Drag.dragEventSource mouseEvent $ \e -> Just $ DragMove dragContext e H.Listening

    DragMove dragContext (Drag.Move mouseEvent dragData) next -> next <$ do
      let
        GraphPosition initialPos = dragContext.initialPos
        newGraphPos = GraphPosition { x : initialPos.x + dragData.offsetX
                                    , y : initialPos.y + dragData.offsetY
                                    }
        PositionLens elementPosLens = dragContext.elementPosLens
      H.modify_ $ set elementPosLens newGraphPos

    DragMove dragContext (Drag.Done mouseEvent) next -> do
      case dragContext.sourceType of
        -- Create edge on halo drop
        HaloDrag -> do
          AppState state <- H.get
          case state.drawingEdgeState of
            Nothing -> pure next
            Just drawingEdgeState -> do
              let
                isWithinHalo = \node ->
                  haloRadius > euclideanDistance node.pos drawingEdgeState.pos
                maybeDropTarget =
                  firstOf (_circles <<< traversed <<< filtered isWithinHalo) (AppState state)
              case maybeDropTarget of
                Nothing -> pure unit
                Just circle -> do
                  let newEdge = { source : drawingEdgeState.source, target : circle.id }
                  H.modify_ $ setJust (_edges <<< at newEdge) unit
              eval $ EndEdgeDraw next
        _ -> pure next

    BeginEdgeDraw dragContext mouseEvent q -> do
      H.liftEffect $ log "Begin edge draw"
      AppState state <- H.get
      let
        mousePagePos = mouseEventPosition mouseEvent
        mouseGraphPos = toGraphPosition mousePagePos (AppState state)
      H.modify_ $ set _drawingEdgeState $ Just { pos : mouseGraphPos
                                               , source : dragContext.elementId
                                               }
      -- Cheeky hack to get drawing edge point to be at cursor:
      -- Shift initial position to where the cursor clicked the halo
      let
        PagePosition initialPagePos = toPagePosition dragContext.initialPos (AppState state)
        PagePosition mousePos = mouseEventPosition mouseEvent
        initialOffset = mousePos - initialPagePos
        GraphPosition circlePos = dragContext.initialPos
        drawEdgeContext = dragContext { initialPos = GraphPosition $ circlePos + initialOffset }
      eval $ q drawEdgeContext

    EndEdgeDraw next -> next <$ do
      H.liftEffect $ log "End edge draw"
      H.modify_ $ set _drawingEdgeState Nothing

    CreateNode mouseEvent next -> next <$ do
      newUUID <- H.liftEffect genUUID
      AppState state <- H.get
      let
        newCirclePos = toGraphPosition (mouseEventPosition mouseEvent) (AppState state)
        newCircle = { id : newUUID
                    , pos : newCirclePos
                    , text : "newnode"
                    , textFieldShape : defaultTextFieldShape
                    }
      H.modify_ $ setJust (_circles <<< at newUUID) newCircle

    DeleteNode id next -> next <$ do
      AppState state <- H.get
      case Map.lookup id state.circles of
        Nothing -> pure unit
        Just circle -> do
          H.liftEffect $ log circle.text
          H.liftEffect $ log $ show id
      let nodeEdges = Set.filter (\edge -> edge.source == id || edge.target == id) state.edges
      H.modify_ $ set (_circles <<< at id) Nothing
      H.modify_ $ set _edges $ Set.difference state.edges nodeEdges
      pure unit

mouseEventPosition :: ME.MouseEvent -> PagePosition
mouseEventPosition e = PagePosition { x : toNumber $ ME.pageX e
                                    , y : toNumber $ ME.pageY e
                                    }

euclideanDistance :: GraphPosition -> GraphPosition -> Number
euclideanDistance (GraphPosition pos1) (GraphPosition pos2) =
  Math.sqrt
  $ (Math.pow (pos1.x - pos2.x) 2.0)
  + (Math.pow (pos1.y - pos2.y) 2.0)
