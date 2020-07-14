port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style, target, href, type_, placeholder, value, checked, disabled)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Http
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (spanishLocale, usLocale)


usdIdrConversion = 14553.85

type Msg 
  = Tick ()
  | GotPrice (Result Http.Error Price)
  | GotTimeLimit (Result Http.Error TimeLimit)
  | GotDonors (Result Http.Error (List Donor))
  | ChangeDonorName String
  | ChangeContribAmount String
  | AddDonor
  | SavedDonor (Result Http.Error String)

port ticker : (() -> msg) -> Sub msg

type CurrencyMode
  = USD
  | IDR

type alias Flag =
  { baseUrl : String 
  , currentTime : Int
  }

type alias Model =
  { timeLimit : Int
  , currentTime : Int
  , price : Int
  , donors : List Donor
  , mode : CurrencyMode
  , baseUrl : String
  , donorName : String
  , contribAmount : Int
  }

type alias Donor =
  { id : Int
  , name : String
  , contrib  : Int
  , paid : Int
  }

donorDecoder =
  Decode.succeed Donor 
    |> required "id" Decode.int
    |> required "name" Decode.string
    |> required "contrib" Decode.int
    |> required "paid" Decode.int

donorEncoder donor =
  Encode.object
    [ ("id", Encode.int donor.id)
    , ("name", Encode.string donor.name)
    , ("contrib", Encode.int donor.contrib) 
    ]

type alias TimeLimit =
  { timeLimit : Int }

timeLimitDecoder =
  Decode.succeed TimeLimit
    |> required "timeLimit" Decode.int

type alias Price =
  { price : Int }

priceDecoder =
  Decode.succeed Price
    |> required "price" Decode.int

initialModel : Model
initialModel =
  { timeLimit = 0
  , currentTime = 0
  , price = 0
  , donors = []
  , mode = IDR
  , baseUrl = ""
  , donorName = ""
  , contribAmount = 0
  }

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : Flag -> ( Model, Cmd Msg )
init flag =
  ( { initialModel 
    | baseUrl = flag.baseUrl 
    , currentTime = flag.currentTime
    }
  , Cmd.batch 
      [ -- Get price
        Http.get
          { url = flag.baseUrl ++ "/price"
          , expect = Http.expectJson GotPrice priceDecoder
          }
      -- Get time limit
      , Http.get
          { url = flag.baseUrl ++ "/timelimit"
          , expect = Http.expectJson GotTimeLimit timeLimitDecoder
          }
      , Http.get
          { url = flag.baseUrl ++ "/donors"
          , expect = Http.expectJson GotDonors (Decode.list donorDecoder)
          } 
      ]
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick _ ->
      let
        newTimeLimit = 
          if model.timeLimit - 1000 < 0 then
            0
          else
            model.timeLimit - 1000

      in
      ( { model | timeLimit = newTimeLimit }, Cmd.none )

    GotPrice res ->
      case res of
        Ok price ->
          ( { model | price = price.price }, Cmd.none )

        _ ->
          ( model, Cmd.none )

    GotTimeLimit res ->
      case res of
        Ok timeLimit ->
          ( { model | timeLimit = timeLimit.timeLimit - model.currentTime }, Cmd.none )

        _ ->
          ( model, Cmd.none )

    GotDonors res ->
      case res of
        Ok donors ->
          ( { model | donors = donors }, Cmd.none )

        _ ->
          ( model, Cmd.none )
        
    ChangeDonorName name ->
      ( { model | donorName = name }, Cmd.none )

    ChangeContribAmount amountStr ->
      let
        amount =
          case String.toInt amountStr of
            Just amt ->
              amt

            Nothing ->
              0 
      in
      ( { model | contribAmount = amount }, Cmd.none )

    AddDonor ->
      let
        donorToAdd : Donor
        donorToAdd =
          { id = 0
          , name = model.donorName
          , contrib = model.contribAmount
          , paid = 0
          }
      in 
      if model.contribAmount == 0 || model.donorName == "" then
        ( model, Cmd.none )
      else
        ( { model | contribAmount = 0, donorName = "" }
        , Http.post
            { url = model.baseUrl ++ "/donors"
            , body = Http.jsonBody (donorEncoder donorToAdd)
            , expect = Http.expectString SavedDonor
            }
        )

    SavedDonor res ->
      case res of
        Ok _ ->
          ( model
          , Http.get
              { url = model.baseUrl ++ "/donors"
              , expect = Http.expectJson GotDonors (Decode.list donorDecoder)
              }  
          )

        _ ->
          ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ ticker Tick ]

view : Model -> Html Msg
view model =
  let
    timeLimitSecs = model.timeLimit // 1000
    days = timeLimitSecs // 86400
    hours = (timeLimitSecs - (days * 86400)) // 3600
    minutes = (modBy 3600 timeLimitSecs) // 60
    secs = modBy 60 timeLimitSecs 
     
  in
  div []
    [ h1 [] [ text "Humblebundle Hunter" ]
    , h2 [] 
        [ text <| 
            "Time limit: " 
            ++ (String.fromInt timeLimitSecs) 
            ++ "(" ++ String.fromInt days 
            ++ "d " 
            ++ String.fromInt hours
            ++ "h "
            ++ String.fromInt minutes
            ++ "m "
            ++ String.fromInt secs
            ++ "s"
            ++ ")"
        ]
    , a 
        [ href "https://www.humblebundle.com/books/circuits-electronics-morgan-claypool-books?hmb_source=humble_home&hmb_medium=product_tile&hmb_campaign=mosaic_section_2_layout_index_5_layout_type_threes_tile_index_1_c_circuitselectronicsmorganclaypool_bookbundle" 
        , target "_blank"
        ]
        [ text "Bundle link" ]
    , h2 [ style "color" "green" ] 
        [ text <| 
            "Price: USD $" 
            ++ format usLocale (toFloat model.price) 
            ++ " (IDR Rp" 
            ++ format usLocale (toFloat model.price * usdIdrConversion) 
            ++ ")" 
        ]
    , div [] [ text "Using 1 USD -> 14,553.85 IDR as per 14th July 2020" ]
    , div 
        [ style "margin-top" "15", style "margin-bottom" "15" ] 
        [ text <| "Please transfer with any amount to " 
        , strong [] [ text "BTPN (bank code: 213) 90015163145" ]
        , text " or via Jenius app with cashtag" 
        , strong [] [ text " $vmasdani" ]
        ]
    , h1 [] 
        [ text <| "Donors: Rp"
            ++ format usLocale (toFloat <| List.foldl (\donor acc -> acc + donor.contrib) 0 model.donors)
            ++ " / Rp"
            ++ format usLocale (toFloat model.price * usdIdrConversion)
        ]
    , div []
        [ text "Be a donor: "
        , input 
            [ type_ "text"
            , placeholder "Name..." 
            , value model.donorName
            , onInput ChangeDonorName
            ] []
        , input 
            [ type_ "number"
            , placeholder "Contrib amount..." 
            , value (String.fromInt model.contribAmount)
            , onInput ChangeContribAmount
            ] []
        , button [ onClick AddDonor ] [ text "Go!" ]
        ]
    , ul []
        (List.map donorListItem model.donors)
    ]

donorListItem : Donor -> Html Msg
donorListItem donor =
  li []
    [ text <|
        donor.name
        ++ ": IDR "
        ++ format usLocale (toFloat donor.contrib)
        ++ " | Paid? "
    , input 
        [ type_ "checkbox" 
        , checked (if donor.paid == 0 then False else True)
        , disabled True
        ] []
    ]