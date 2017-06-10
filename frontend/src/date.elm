module Demo exposing (main)

import Html exposing (Html, text, p, label, form, ul, li, div, select, option, button, table, tr, td, tbody, th, input)
import Html.Attributes exposing (selected, value, disabled, type_)
import Html.Events exposing (onInput, onClick)
import DateTimePicker
import DateTimePicker.Config exposing (Config, DatePickerConfig, TimePickerConfig, defaultDatePickerConfig, defaultDateTimePickerConfig, defaultDateTimeI18n, defaultTimePickerConfig)
import Date exposing (Date)
import Css
import DateTimePicker.Css
import DatepickerCss exposing (CssClasses(..))
import Html.CssHelpers
import Date.Extra.Format
import Date.Extra.Config.Config_en_au exposing (config)
import DateParser


form2 : Model -> List (Html Msg)
form2 model = 
   let
       leftovers = (100 - (List.sum (List.map Tuple.second model.parties)))
   in
       [ field "Start Date" (renderDate model.startDateState model.startDate StartDateChanged)
       , field "End Date" (renderDate model.endDateState model.endDate EndDateChanged)
       , field "Publisher" (renderPublisherSelect model.publisher SetPublisher)
       , field "Govt 2PP" (input [onInput Set2PP, value model.govt2pp] [])
       , field "New Party" (renderNewParty model.parties model.partialParty model.partialPartyAmount model.partialPartyAmountParsed SetPartialParty OnCompleteParty) ] ++ List.map renderParty model.parties ++ 
       [ field "(Leftovers)" (text (toString leftovers))
       , field "" (renderAddButton model leftovers)]

renderAddButton model leftovers = 
   let
       clickHandler = Maybe.withDefault 
                         NoOp 
                         (Maybe.map4 AddPoll model.startDate model.endDate model.publisher (resultToMaybe (String.toFloat model.govt2pp)) )
   in
      button [ type_ "button", onClick clickHandler, disabled (clickHandler == NoOp || leftovers > 1) ] [text "Add Poll"]




renderDate : DateTimePicker.State -> Maybe Date -> (DateTimePicker.State -> Maybe Date -> Msg) -> Html Msg
renderDate state date setter = DateTimePicker.datePickerWithConfig
                            (customI18nConfig setter)
                            []
                            state
                            date

renderPublisherSelect = flip selectPublisherBox 

renderParty (party, amount) = field (partyToName party) (div [] 
   [ (text (toString amount))
   , (text " ")
   , (button [type_ "button", onClick (RemoveParty party)] [text "❌"])
   ])

renderNewParty parties pp ppa ppap setter onCompleteParty = 
   let
       otherParties : List (Maybe Party, String)
       otherParties = (List.filter (bubble >> Maybe.map (partyUsed (List.map Tuple.first parties)) >> Maybe.withDefault True) partyEntries)
       theButton = button [ type_ "button"
            , onClick (Maybe.withDefault NoOp (Maybe.map2 onCompleteParty pp ppap))
            , disabled ((not (maybeToBoolean ppap)) || (not (maybeToBoolean pp)))
            ] [text ("Complete (" ++ (toString ppap) ++ ")")]
   in
   div []
   [ selectPartyBox otherParties (\e -> setter e ppa) pp
   , input [value ppa, onInput (setter pp)] []
   , theButton
   ]



partyUsed : List Party -> (Party, String) -> Bool
partyUsed parties (p, _) = not (List.member p parties)

bubble : (Maybe a, b) -> Maybe (a, b)
bubble (a, b) = case a of
        Nothing -> Nothing
        Just a_ -> Just (a_, b)

maybeToBoolean m = case m of 
   Just _ -> True
   Nothing -> False

field : String -> Html msg -> Html msg
field labelText content = tr [] 
        [ th [] [label [] [text labelText]]
        , td [] [content]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Firm = Essential | Newspoll | Ipsos
publisherFromString : String -> Maybe Firm
publisherFromString a = case a of
        "Nothing" -> Nothing
        "Just Essential" -> Just Essential
        "Just Newspoll" -> Just Newspoll
        "Just Ipsos" -> Just Ipsos
        _ -> Nothing

publisherEntries = [ Nothing => "...",
        Just Essential => "Essential",
        Just Newspoll => "Newspoll",
        Just Ipsos => "Ispsos"
        ]
        

type Party = LNC | ALP | LIB | NAT | GRN | ONP | OTH
partyFromString : String -> Maybe Party
partyFromString a = case a of
        "Nothing" -> Nothing
        "Just LNC" -> Just LNC
        "Just ALP" -> Just ALP
        "Just LIB" -> Just LIB
        "Just NAT" -> Just NAT
        "Just GRN" -> Just GRN
        "Just ONP" -> Just ONP
        "Just OTH" -> Just OTH
        _ -> Nothing

partyToName party = case party of
        LNC -> "Coalition"
        ALP -> "Labor"
        LIB -> "Liberals"
        NAT -> "Nationals"
        GRN -> "Greens"
        ONP -> "One Nation"
        OTH -> "Other"

partyEntries = [
        Nothing => "...",
        Just LNC => "Coalition",
        Just ALP => "Labor",
        Just LIB => "Liberals",
        Just NAT => "Nationals",
        Just GRN => "Greens",
        Just ONP => "One Nation",
        Just OTH => "Other"
        ]

type alias Poll = 
   { startDate: Date
   , endDate: Date
   , publisher: Firm
   , govt2pp: Float
   , parties: List (Party, Float)
   , isNew: Bool
   }

type alias Model =
    { startDate : Maybe Date
    , startDateState : DateTimePicker.State
    , endDate : Maybe Date
    , endDateState : DateTimePicker.State
    , publisher: Maybe Firm
    , govt2pp: String
    , parties: List (Party, Float)
    , partialParty: Maybe Party
    , partialPartyAmount: String
    , partialPartyAmountParsed: Maybe Float
    , polls: List Poll
    }

initialState : Model
initialState = { startDate = Nothing
      , startDateState = DateTimePicker.initialState
      , endDate = Nothing
      , endDateState = DateTimePicker.initialState
      , publisher = Nothing
      , govt2pp = ""
      , parties = []
      , partialParty = Nothing
      , partialPartyAmount = ""
      , partialPartyAmountParsed = Nothing
      , polls = []
      }

init : ( Model, Cmd Msg )
init =
    ( initialState
    , Cmd.batch
        [ DateTimePicker.initialCmd StartDateChanged DateTimePicker.initialState
        , DateTimePicker.initialCmd EndDateChanged DateTimePicker.initialState
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


{ id, class, classList } =
    Html.CssHelpers.withNamespace ""



customI18nConfig : (DateTimePicker.State -> (Maybe Date) -> Msg) -> Config (DatePickerConfig {}) Msg
customI18nConfig a =
    let
        defaultDateConfig =
            defaultDatePickerConfig a
    in
        { defaultDateConfig
            | i18n = { defaultDateTimeI18n | inputFormat = customInputFormat }
        }


customDatePattern : String
customDatePattern =
    "%d.%m.%Y"


customInputFormat : DateTimePicker.Config.InputFormat
customInputFormat =
    { inputFormatter = Date.Extra.Format.format config customDatePattern
    , inputParser = DateParser.parse config customDatePattern >> Result.toMaybe
    }


view : Model -> Html Msg
view model =
    let
        { css } =
            Css.compile [ DateTimePicker.Css.css, DatepickerCss.css ]
    in
        form []
            [ Html.node "style" [] [ Html.text css ]
            , div [ class [ Container ] ]
                  ([table [] [tbody [] (form2 model)]] ++
                  [ p [] [text (toString model)]])
            ]

(=>) = (,)

makeOption current e = 
        let code = Tuple.first e 
        in option [value (toString code), selected (code == current)] [text (Tuple.second e)]

selectBox : (String -> a) -> List (a, String) -> (a -> Msg) -> a -> Html Msg
selectBox deserialiser options setter current =
   select [onInput (setter << deserialiser)] (List.map (makeOption current) options)

selectPublisherBox = selectBox publisherFromString publisherEntries 
selectPartyBox entries = selectBox partyFromString entries

type Msg
    = NoOp
    | StartDateChanged DateTimePicker.State (Maybe Date)
    | EndDateChanged DateTimePicker.State (Maybe Date)
    | SetPublisher (Maybe Firm)
    | SetPartialParty (Maybe Party) String
    | OnCompleteParty Party Float
    | Set2PP String
    | AddPoll Date Date Firm Float
    | RemoveParty Party

resultToMaybe result = case result of
   Ok r -> Just r
   Err _ -> Nothing

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartDateChanged state value ->
            ( { model | startDate = value, startDateState = state }, Cmd.none )

        EndDateChanged state value ->
            ( { model | endDate = value, endDateState = state }, Cmd.none )

        SetPublisher firm ->
            ( { model | publisher = firm}, Cmd.none )

        SetPartialParty party amount ->
            ( { model | partialParty = party, partialPartyAmount = amount, partialPartyAmountParsed = resultToMaybe (String.toFloat amount) }, Cmd.none )

        OnCompleteParty party amount -> 
            ( { model | parties = model.parties ++ [party => amount], partialParty = Nothing, partialPartyAmount = "", partialPartyAmountParsed = Nothing }, Cmd.none)

        Set2PP amount ->
           ( {model | govt2pp = amount}, Cmd.none)

        RemoveParty party ->
           ( { model | parties = List.filter (Tuple.first >> (/=) party) model.parties }, Cmd.none)

        AddPoll startDate endDate publisher govt2pp  ->
           ( { initialState | polls = model.polls ++ [
              { startDate = startDate
              , endDate = endDate
              , publisher = publisher
              , govt2pp = govt2pp
              , parties = model.parties
              , isNew = True
           }]
          } , Cmd.none)

