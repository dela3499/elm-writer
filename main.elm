port module WordCounter exposing ( .. )

import Html exposing ( Html, text, div, textarea )
import Html.Events exposing ( onInput )
import Html.Attributes exposing ( .. )
import Html.App as Html

import List
import String
import Time exposing ( .. )


type Msg 
  = SetContent String
  | Tick Time

type alias Model =
  { initialTime: Float
  , secondsElapsed: Float
  , content: String
  }

initialModel = 
  { initialTime = 0
  , secondsElapsed = 0
  , content = ""
  }

subscriptions: Model -> Sub Msg
subscriptions model = 
  every ( 100 * millisecond ) ( \x -> Tick ( inSeconds x ) )

main = 
  Html.program
    { init = ( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetContent content -> 
      ( { model | content = content }, Cmd.none )
    Tick t -> 
      if model.initialTime == 0
        then 
          ( { model 
                | initialTime = t - 1 
                , secondsElapsed = 1
            }, Cmd.none )
        else 
          ( { model | secondsElapsed = t - model.initialTime }, Cmd.none )

view : Model -> Html Msg
view model =
  div 
    [ id "container" ]
    [ textarea
        [ class "content"
        , placeholder "Type in here!"
        , onInput SetContent
        ]
        []
    , div 
        [ class "infoContainer" ]
        [ div
          [ class "timer" ]
          [ text ( formatTime model.secondsElapsed ) ]
        , div
          [ class "wordcount" ]
          [ text ( model.content |> countWords |> formatCount ) ]
        ]
    ]

formatTime seconds = 
  let nMinutes = floor ( seconds / 60 )
      nSeconds = seconds - ( nMinutes * 60 |> toFloat ) |> floor
      minutesString = toString nMinutes
      secondsString =
        if nSeconds < 10
          then "0" ++ ( toString nSeconds )
          else toString nSeconds
  in minutesString ++ ":" ++ secondsString

countWords string = 
  string
  |> String.words
  |> List.filter ( \x -> x /= "" )
  |> List.length

formatCount count = ( toString count ) ++ " words"
