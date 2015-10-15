type alias Model =
  { left : Random.Model
  , right : Random.Model
  }

init : String -> (Model, Effets Action)
init leftTopic rightTopic
  let
    (left, leftFx) = RandomGif.init leftTopic
    (right, rightFx) = RandomGif.init rightTopic
  in
    ( Model left right
    , Effects.batch
      [Effects.map Left leftFx
      , Effects.map Right rightFx
      ]
    )

module Effects where

type Effets a

none : Effets a

task : Task Never a -> Effects a

type Action
    = Left RandomGif.Action
    | Right RandomGif.Action

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Left msg ->
      let
        (left, fx) = RandomGif.update msg model.left
      in
        ( Model left model.right
        , Effects.map Left fx
        )
    Right msg ->
      let
        (right, fx) = RandomGif.update msg model.right
      in
        ( Model model.left right
        , Effects.map Right fx
        )


getRandomGif : String -> Effects Action
getRandomGif topic =
    Http.get decodeImageUrl (randomUrl topic)
      |> Task.toMaybe
      |> Task.map NewGif
      |> Effets.task

randomUrl: String -> String
randomUrl topic =
  Http.url

decodeImageUrl : Json.Decoder String
decodeImageUrl = Json.at ["data", "image_url"] Json.string
