module Circles where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (Maybe(..))
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
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.KeyboardEvent as KE
import Web.DOM.Node as DN
import Web.DOM.NodeList as NL
import Web.Event.Event as WE

import Data.UUID (UUID)

type WindowSize = { width :: Int, height :: Int }


type CircleId = UUID
type CircleState = { id :: UUID, pos :: Drag.PageCoord, text :: String }

initialState :: CircleState -> CircleState
initialState = identity

data Query a =
    DragStart CircleId ME.MouseEvent a
  | DragMove Drag.DragEvent a
  | DropTarget (Maybe CircleId) a
  | TextInput CircleId KE.KeyboardEvent a
  | DeleteNode CircleId a
  | PreventDefault WE.Event (Query a)
  | StopPropagation WE.Event (Query a)
  | TestLog String a


data Message =
    Drag CircleId Drag.DragData
  | Mouseover (Maybe CircleId)
  | DragEnd CircleId
  | Delete CircleId

circleNode :: H.Component HH.HTML Query CircleState Message Aff
circleNode =
  H.component
    { initialState : initialState
    , render : render
    , eval : eval
    , receiver : const Nothing
    }
  where

  render :: CircleState -> H.ComponentHTML Query
  render state =
    let
      circleView = \circleState -> SE.g []
                                   [ SE.circle
                                     [ HP.id_ $ show circleState.id
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
      in
        circleView state

  eval :: Query ~> H.ComponentDSL CircleState Query Message Aff
  eval = case _ of
    PreventDefault e q -> do
      H.liftEffect $ WE.preventDefault e
      eval q
    StopPropagation e q -> do
      H.liftEffect $ WE.stopPropagation e
      eval q
    DragStart circleId mouseEvent next -> next <$ do
      H.subscribe $ Drag.dragEventSource mouseEvent $ \e -> Just $ DragMove e H.Listening
    DragMove dragEvent next -> next <$ do
      state <- H.get
      case dragEvent of
        Drag.Move mouseEvent dragData -> do
          H.modify_ _{ pos = Drag.mouseEventToPageCoord mouseEvent }
        Drag.Done mouseEvent -> do
          H.raise $ DragEnd state.id
    DropTarget maybeTarget next -> next <$ do
      H.raise $ Mouseover maybeTarget
    TextInput graphNodeId keyboardEvent next -> next <$ do
      let event = KE.toEvent keyboardEvent
      let maybeNode = WE.target event >>= DN.fromEventTarget
      case maybeNode of
        Nothing -> pure unit
        Just node -> do
          nodeText <- H.liftEffect $ getContentEditableText node
          H.modify_ _{ text = nodeText}
    DeleteNode circleId next -> next <$ do
      H.raise $ Delete circleId
    TestLog str next -> next <$ do
      H.liftEffect $ log str

getContentEditableText :: DN.Node -> Effect String
getContentEditableText node = do
  childrenNodeList <- DN.childNodes node
  children <- NL.toArray childrenNodeList
  nodeText <- traverse DN.textContent children
  pure $ String.joinWith "\n" nodeText
