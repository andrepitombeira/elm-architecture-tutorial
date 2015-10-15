module CounterList where

import Counter
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
  { counters : List ( ID, Counter.Model )
  , nextID: ID
  }

type alias ID = Int

type Action
  = Insert
  | Remove ID
  | Modify ID Counter.Action

update : Action -> Model -> Model
update action model =
  case action model of
    Insert ->
      let newCounter = ( model.nextID, Counter.init 0 )
          newCounters = model.counters ++ [ newCounter ]
      in
        { model |
          counters <- newCounters
          nextID <- model.nextID + 1
        }

    Remove ->
      { model |
        counters <- List.drop 1,
        nextID <- model.counters
      }

    Modify id counterAction ->
      let updateCounter (counterID, counterModel) =
        if counterID == id
          then (counterID, Counter.update counterAction counterModel)
          else (counterID, counterModel)
      in
        { model | counters <- List.map updateCounter model.counters }

view : Signal.Address Action -> Model -> Html
view address model =
  let insert = button [ onCLick address Insert ] [ text "Add" ]
  in
    div [] (insert :: List.map (viewCounter address) model.counters)

viewCounter : Signal.Address Action -> (ID, Counter.Model) -> Html
viewCounter address (id, model) =
  let context =
    Counter.Context
      (Signal.forwardTo address (Modify id))
      (Signal.forwadTo address (always (Remove id)))
  in
    Counter.viewWithRemoveButton context model
