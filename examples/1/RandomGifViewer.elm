type alias Model =
  { topic : String
  , gifUrl : String
  }

init : String -> (Model, Effets Action)
update : Action -> Model -> (Model, Effects Action)

module Effects where

type Effets a

none : Effets a

task : Task Never a -> Effects a

type Action
    = RequestMore
    | NewGif (Maybe String)

update : Action -> Model -> (Model, Effects Action)
update msg model =
  case msg of
    RequestMore ->
      ( model
      , getRandomGif model.topic
      )

    NewGif mayberUrl ->
      ( Model model.topic (Maybe.withDefault model.gifUrl mayber)
      , Effects.none
      )

getRandomGif : String -> Effects Action
getRandomGif topic =
    Http.get decodeImageUrl (randomUrl topic)
      |> Task.toMaybe
      |> Task.map NewGif
      |> Effets.task

randomUrl: String -> String
randomUrl topic =
  Http.url "http://api.giphy.com/v1/gifs/random"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "tag" => topic
    ]

decodeImageUrl : Json.Decoder String
decodeImageUrl = Json.at ["data", "image_url"] Json.string
