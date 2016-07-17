import Html exposing (Html, Attribute, ul, li, div, input, text, button)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick,onInput)
import String exposing (toInt)

main =
  Html.beginnerProgram { model = model, update = update, view = view }

-- MODEL

type alias Model =
  { state : Bool,
    beats: Int,
    playing: Bool,
    uid : Index,
    nodes: List Note
  }

model : Model
model =
  Model False 0 False 0 (List.repeat 8 (Note False))

type alias Index = Int

type alias Track = {
  nodes: List Note
}


type alias Note = {
  state: Bool
  }


type alias Nodes = List Note

-- UPDATE

type Action = CalculateGrid String | Play | UpdateNote Int Bool

update : Action -> Model -> Model
update action model =
  case action of
      Play ->
      Debug.log "Playing"
       { model | playing = not model.playing}
      UpdateNote index state ->
        let
          updateNote ix note =
            if ix == index then { note | state = not state } else note
        in
      Debug.log "UpdateNote"
          { model | nodes = List.indexedMap updateNote model.nodes}
      CalculateGrid str ->
        let
          newInt = Result.withDefault 0 (String.toInt str)
        in
        Debug.log "CalculateGrid"
          { model | nodes = (List.repeat newInt ({state = False}))}

-- VIEW

view : Model -> Html Action
view model =  div [] [
  button [onClick Play] [text "Play"],
  input  [placeholder "# of nodes", onInput CalculateGrid] [],
  renderNodes model.nodes
 ]

renderNodes: List Note -> Html Action
renderNodes nodes =
  let
    nodeList = List.indexedMap renderNode nodes
  in
    ul [ class "grid" ] nodeList

renderNode: Index -> Note -> Html Action
renderNode i note =
  li [ class "node" ] [
      button [onClick (UpdateNote i note.state)] [text (toString note.state)]
   ]





