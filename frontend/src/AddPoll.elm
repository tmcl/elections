module AddPoll exposing (main)

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.

# Definition
@docs main

-}

import Http
import Html exposing (Html, text, p, label, form, ul, li, div, select, option, button, table, tr, td, tbody, thead, th, input)
import Html.Attributes exposing (selected, value, disabled, type_)
import Html.Events exposing (onInput, onClick)
import DateTimePicker
import DateTimePicker.Config exposing (Config, DatePickerConfig, TimePickerConfig, defaultDatePickerConfig, defaultDateTimePickerConfig, defaultDateTimeI18n, defaultTimePickerConfig)
import Date exposing (Date, year, month, day)
import Css
import DateTimePicker.Css
import DatepickerCss exposing (CssClasses(..))
import Html.CssHelpers
import Date.Extra.Format
import Date.Extra.Config.Config_en_au exposing (config)
import DateParser
import Json.Encode
import Json.Decode exposing (Decoder)


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
       , field "" (renderAddButton model leftovers)
       , field "Other polls" (otherPolls model.polls)]

otherPolls : List Poll -> Html Msg
otherPolls polls = 
   table []
        [ thead [] 
                [ tr []
                        [ th [] [text "Date"] 
                        , th [] [text "Publisher"] 
                        , th [] [text "Govt 2PP"] 
                        , th [] [text "LNC"] 
                        , th [] [text "ALP"]
                        , th [] [text "GRN"]
                        , th [] [text "ONP"]
                        , th [] [text "OTH"] 
                        ]
                ]
        , tbody [] (List.concat (List.map otherPoll polls))
        ]

formatDates : Date -> Date -> String
formatDates start end = if start == end then formatDate start else formatDate start ++ " - " ++ formatDate end

formatDate : Date -> String
formatDate date = toString (day date) ++ " " ++ toString (month date) ++ " " ++ toString (year date)
   
otherPoll : Poll -> List (Html msg)
otherPoll poll = [ tr []
                        [ td [] [text (formatDates poll.startDate poll.endDate)] 
                        , td [] [text (toString poll.publisher)] 
                        , td [] [text (toString poll.govt2pp)] 
                        , td [] [text (toString (getGroup poll LNC))] 
                        , td [] [text (toString (getGroup poll ALP))]
                        , td [] [text (toString (getGroup poll GRN))]
                        , td [] [text (toString (getGroup poll ONP))]
                        , td [] [text (toString (getGroup poll OTH))] 
                        ]
                ]

getParty : List (Party, Float) -> Party -> Float
getParty results party = List.sum (List.map Tuple.second (List.filter (\e -> Tuple.first e == party) results))

getGroup : Poll -> Party -> Float
getGroup poll party = case party of
   LNC -> (getParty poll.parties LNC) + (getParty poll.parties LIB) + (getParty poll.parties NAT)
   _ -> getParty poll.parties party

renderAddButton : Model -> comparable -> Html Msg
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

renderPublisherSelect : Maybe Firm -> (Maybe Firm -> Msg) -> Html Msg
renderPublisherSelect = flip selectPublisherBox 

renderParty : ( Party, a ) -> Html Msg
renderParty (party, amount) = field (partyToName party) (div [] 
   [ (text (toString amount))
   , (text " ")
   , (button [type_ "button", onClick (RemoveParty party)] [text "❌"])
   ])

renderNewParty
    : List ( Party, a2 )
    -> Maybe Party
    -> String
    -> Maybe b
    -> (Maybe Party -> String -> Msg)
    -> (Party -> b -> Msg)
    -> Html Msg
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

maybeToBoolean : Maybe a -> Bool
maybeToBoolean m = Maybe.withDefault False (Maybe.map (\_ -> True) m)

field : String -> Html msg -> Html msg
field labelText content = tr [] 
        [ th [] [label [] [text labelText]]
        , td [] [content]
        ]



{-| aoeu |-}
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

publisherEntries : List ( Maybe Firm, String )
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

partyToName : Party -> String
partyToName party = case party of
        LNC -> "Coalition"
        ALP -> "Labor"
        LIB -> "Liberals"
        NAT -> "Nationals"
        GRN -> "Greens"
        ONP -> "One Nation"
        OTH -> "Other"

partyEntries : List ( Maybe Party, String )
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

oldPoll poll = {poll | isNew = False}

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

(=>) : a -> b -> ( a, b )
(=>) = (,)

makeOption : a -> ( a, String ) -> Html msg
makeOption current e = 
        let code = Tuple.first e 
        in option [value (toString code), selected (code == current)] [text (Tuple.second e)]

selectBox : (String -> a) -> List (a, String) -> (a -> Msg) -> a -> Html Msg
selectBox deserialiser options setter current =
   select [onInput (setter << deserialiser)] (List.map (makeOption current) options)

selectPublisherBox : (Maybe Firm -> Msg) -> Maybe Firm -> Html Msg
selectPublisherBox = selectBox publisherFromString publisherEntries 

selectPartyBox
    : List ( Maybe Party, String )
    -> (Maybe Party -> Msg)
    -> Maybe Party
    -> Html Msg
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
    | ReceiveUpdates (Result Http.Error Success)

resultToMaybe : Result error a -> Maybe a
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
           let newPoll = { startDate = startDate
              , endDate = endDate
              , publisher = publisher
              , govt2pp = govt2pp
              , parties = model.parties
              , isNew = True
           }
           in
           ({initialState | polls = model.polls ++ [newPoll]}, submitPoll newPoll)

        ReceiveUpdates (Ok _) -> (model, Cmd.none) {- todo -}
        ReceiveUpdates (Err _) -> (model, Cmd.none) {- todo -}

{-
pollDecoder : Decoder Poll
pollDecoder = Json.Decode.map4 oldPoll
        (Json.Decode.at ["startDate", "endDate"] Json.Decode.date)
        (Json.Decode.at ["publisher"] firmDecoder)
        (Json.Decode.at ["govt2pp"] Json.Decode.float)
        (Json.Decode.at ["parties"] (list partyDecoder))

firmDecoder : Decoder Firm
firmDecoder = string |> Json.Decode.andThen helpFirm

helpFirm : String -> Decoder Firm
helpFirm firm = 
   case firm of
      "Essential" -> Json.Decode.succeed Essential
      "Newspoll" -> Json.Decode.succeed Newspoll
      "Ipsos" -> Json.Decode.success Ipsos
      e -> Json.Decode.fail ("Unknown firm in json" ++ e)

partyDecoder : Decoder Party
partyDecoder = 
   -}
   
successDecoder : Decoder Success
successDecoder = Json.Decode.string |> Json.Decode.andThen helpSuccess

type Success = Good | Bad

helpSuccess : String -> Decoder Success
helpSuccess success = 
   case success of
      "Good" -> Json.Decode.succeed Good
      "Bad" -> Json.Decode.succeed Bad
      e -> Json.Decode.fail ("Unknown completion in json" ++ e)

pollToJson poll = 
   Json.Encode.object [ "startDate" => encodeDate poll.startDate
                      , "endDate" => encodeDate poll.endDate
                      , "publisher" => Json.Encode.string (toString poll.publisher)
                      , "govt2pp" => Json.Encode.float (poll.govt2pp)
                      , "parties" => Json.Encode.list (List.map encodeParty poll.parties) 
                      ]

encodeDate date = Json.Encode.string ((toString (year date)) ++ "-" ++ 
                                      (toString (month date)) ++ "-" ++
                                      (toString (day date)) ++ "-")

encodeParty = Json.Encode.string << toString

submitPoll : Poll -> Cmd Msg
submitPoll poll = 
   let
       url = "https://localhost:8181/submit-poll"
       body = Http.jsonBody (pollToJson poll)
       request = Http.post url body successDecoder
   in
      Http.send ReceiveUpdates request
