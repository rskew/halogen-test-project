module ContentEditable where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.DOM.Element as DE
import Web.DOM.Node as DN
import Web.DOM.NodeList as NL
import Web.DOM.Document (createElement)
import Web.HTML as WH
import Web.HTML.HTMLElement (toNode, fromElement, toElement)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.Event.Event as WE
import Web.UIEvent.KeyboardEvent as KE

type State = String

data Query a =
    Init a
  | TextInput KE.KeyboardEvent a
  | GetScrollShape (Maybe { width :: Number, height :: Number } -> a)

data Message = TextUpdate String

contenteditable :: H.Component HH.HTML Query State Message Aff
contenteditable =
  H.lifecycleComponent
    { initialState : identity
    , render : render
    , eval : eval
    , receiver : const Nothing
    , initializer : Just $ H.action Init
    , finalizer : Nothing
    }
  where

  editorRef :: H.RefLabel
  editorRef = H.RefLabel "editor"

  render :: State -> H.ComponentHTML Query
  render _ =
    HH.div
    [ HH.attr (HH.AttrName "xmlns") "http://www.w3.org/1999/xhtml"
      , HP.class_ $ HH.ClassName "text-field"
      , HH.attr (HH.AttrName "contenteditable") "true"
      , HE.onKeyUp $ HE.input $ TextInput
      , HP.ref editorRef
      ]
    []

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval = case _ of
    -- Update DOM directly to initalise the text of the contenteditable component.
    -- We can't render the text normally as the contenteditable component keeps its
    -- own state and updates its text outside of Halogen's control.
    Init next -> next <$ do
      state <- H.get
      maybeRef <- H.getHTMLElementRef editorRef
      case maybeRef of
        Nothing -> pure unit
        Just element -> do
          let node = toNode element
          H.liftEffect $ setText state node
    TextInput keyboardEvent next -> next <$ do
      let event = KE.toEvent keyboardEvent
      let maybeNode = WE.target event >>= DN.fromEventTarget
      case maybeNode of
        Nothing -> pure unit
        Just node -> do
          nodeText <- H.liftEffect $ getContentEditableText node
          H.put nodeText
          H.raise $ TextUpdate nodeText
    GetScrollShape reply -> do
      maybeRef <- H.getHTMLElementRef editorRef
      case maybeRef of
        Nothing -> pure $ reply Nothing
        Just element -> do
          scrollWidth <- H.liftEffect $ DE.scrollWidth $ toElement element
          scrollHeight <- H.liftEffect $ DE.scrollHeight $ toElement element
          pure $ reply $ Just { width : scrollWidth + 10.0, height : scrollHeight + 10.0 }

setText :: String -> DN.Node -> Effect Unit
setText text editorNode =
  let
    lines = String.split (String.Pattern "\n") text
  in do
    window <- WH.window
    documentHTML <- document window
    let document = toDocument documentHTML
    traverse_
      (\line -> do
          newDiv <- createElement "div" document
          case fromElement newDiv of
            Nothing -> pure unit
            Just newDivHTMLElement ->
              let newNode = toNode newDivHTMLElement in do
              DN.setTextContent line newNode
              _ <- DN.appendChild newNode editorNode
              pure unit
      )
      lines

getContentEditableText :: DN.Node -> Effect String
getContentEditableText node = do
  childrenNodeList <- DN.childNodes node
  children <- NL.toArray childrenNodeList
  nodeText <- traverse DN.textContent children
  pure $ String.joinWith "\n" nodeText
