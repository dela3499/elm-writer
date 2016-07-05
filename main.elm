port module WordCounter exposing ( .. )

import Html exposing ( Html, text, div, textarea, i )
import Html.Events exposing ( onInput, onClick )
import Html.Attributes exposing ( .. )
import Html.App as Html

import Maybe exposing ( withDefault )
import Array exposing ( Array )
import List
import String
import Time exposing ( .. )

{--
TODO: Need to change wordcount approach for large wordcounts. Can't 
      naively count everything on every keystroke. Seems like there could 
      be some kind of line-diffing, or chunking approach. Maybe use hybrid approach. 
      Once every few seconds, do a full count. Use chunked count every keystroke. 
      Only problematic when making deep edits. 

--}

type Msg 
  = SetContent String
  | Tick Time
  | NextStyle
  | PreviousStyle

type alias Model =
  { initialTime: Float
  , secondsElapsed: Float
  , content: String
  , styleIndex: Int
  , style: String
  }

initialModel = 
  { initialTime = 0
  , secondsElapsed = 0
  , content = ""
  , styleIndex = 0
  , style = Array.get 0 styles |> withDefault defaultStyle
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
    
    NextStyle -> 
      let newIndex = ( model.styleIndex + 1 ) % nStyles 
      in 
        ( { model 
            | styleIndex = newIndex
            , style = Array.get newIndex styles |> withDefault defaultStyle
          } , Cmd.none )
    
    PreviousStyle -> 
      let newIndex = ( model.styleIndex - 1 ) % nStyles 
      in 
        ( { model 
            | styleIndex = newIndex
            , style = Array.get newIndex styles |> withDefault defaultStyle
          } , Cmd.none )

view : Model -> Html Msg
view model =
  let count = countWords model.content
  in
    div 
      [ id "container" 
      , class model.style ]
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
              [ text ( count |> formatCount ) ]
          , div
              [ class "wpm" ]
              [ text ( computeWpm count model.secondsElapsed |> formatWpm ) ]
          , i 
              [ class "fa fa-angle-right next" 
              , onClick NextStyle ]
              []
          , div 
              [ class "styleText" ]
              [ text ( formatStyleIndex model.styleIndex ) ]
          , i 
              [ class "fa fa-angle-left previous" 
              , onClick PreviousStyle ]
              []
          ]
      ]

formatStyleIndex i = 
  "Style " ++ ( toString ( i + 1 ) ) ++ " (of " ++ ( toString nStyles ) ++ ")"

computeWpm words' seconds =
  let words = toFloat words'
  in words / ( seconds / 60 ) |> floor

formatWpm wpm = 
  ( toString wpm ) ++ " wpm"

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

defaultStyle = "dark"

styles = Array.fromList [ "dark", "light", "blue" ]

nStyles = Array.length styles
