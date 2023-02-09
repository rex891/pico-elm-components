module Main exposing (..)

import Browser exposing (Document)
import File
import File.Select
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
    exposing
        ( attribute
        , checked
        , class
        , css
        , disabled
        , placeholder
        , required
        , selected
        , step
        , type_
        , value
        )
import Html.Styled.Events exposing (onCheck, onClick, onInput, stopPropagationOn)
import Json.Decode as De
import List.Extra exposing (remove)
import Tailwind.Utilities exposing (..)


type alias Model =
    { mesg : String
    , selectedSize : Size
    , sizeDropdown : Size
    , lastName : String
    , searchStr : String
    , agree : Bool
    , rangeVal : Float
    , modalOpen : Bool
    , color : String
    , selectedSizes : List Size
    }


init : De.Value -> ( Model, Cmd Msg )
init flags =
    ( { mesg = "PICO"
      , selectedSize = Small
      , sizeDropdown = Small
      , lastName = ""
      , searchStr = ""
      , agree = False
      , rangeVal = 50
      , modalOpen = False
      , color = "#323f2a"
      , selectedSizes = []
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | PrintStr String
    | SetSize Size
    | SetSearch String
    | SetSizeStr (Maybe Size)
    | SetAgree Bool
    | SetLastName String
    | SetRangeVal Float
    | OpenModal Bool
    | ChooseFile
    | GotFile File.File
    | SetColor String
    | SetCheck Size Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PrintStr str ->
            let
                _ =
                    Debug.log "PrintStr" str
            in
            ( model, Cmd.none )

        SetCheck size checked ->
            ( { model
                | selectedSizes =
                    if checked then
                        size :: model.selectedSizes

                    else
                        remove size model.selectedSizes
              }
            , Cmd.none
            )

        SetColor color ->
            ( { model | color = color }, Cmd.none )

        ChooseFile ->
            ( model, File.Select.file [] GotFile )

        GotFile file ->
            ( model, Cmd.none )

        OpenModal which ->
            ( { model | modalOpen = which }, Cmd.none )

        SetRangeVal rangeVal ->
            ( { model | rangeVal = rangeVal }, Cmd.none )

        SetAgree checked ->
            ( { model | agree = checked }, Cmd.none )

        SetSize size ->
            ( { model | selectedSize = size }, Cmd.none )

        SetSizeStr mSize ->
            case mSize of
                Just size ->
                    ( { model | sizeDropdown = size }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetLastName name ->
            ( { model | lastName = name }, Cmd.none )

        SetSearch search ->
            ( { model | searchStr = search }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = model.mesg
    , body =
        [ toUnstyled <|
            div
                [ css
                    [ h_full
                    , p_4
                    ]
                , class
                    (if model.modalOpen then
                        "modal-is-open modal-is-opening"

                     else
                        "modal-is-closing"
                    )

                -- , attribute "data-theme" "light"
                ]
                [ node "hgroup"
                    []
                    [ h1 [] [ text "wassup" ]
                    , h2 [] [ text "dog" ]
                    ]
                , label [] [ text "First name" ]
                , input
                    [ placeholder "First name"
                    , required True
                    , value "Joe Mama"

                    -- , attribute "aria-invalid" "true"
                    ]
                    []
                , label [] [ text "Last name" ]
                , input [ placeholder "Last name", required True, onInput SetLastName ] []
                , label [] [ text "Email address" ]
                , input [ type_ "email", placeholder "Email address", required True ] []
                , small [] [ text "We'll never share your email with anyone else." ]
                , button [ class "outline contrast" ] [ text "Submit" ]
                , div []
                    [ radioGroup "Size" SetSize model.selectedSize sizes
                    , dropdown "Size" SetSizeStr model.sizeDropdown sizes
                    , checkboxDropdown SetCheck model.selectedSizes sizes
                    , label []
                        [ input [ type_ "checkbox", onCheck SetAgree ] []
                        , text "I agree to the Terms and Conditions"
                        ]
                    , label []
                        [ input [ type_ "checkbox", disabled True, checked True ] []
                        , text "I agree to share my information with partners"
                        ]
                    , label [ css [ m_4 ] ]
                        [ input [ type_ "checkbox", attribute "role" "switch" ] []
                        , text "Publish on my profile"
                        ]
                    , label [ css [ m_4 ] ]
                        [ input [ type_ "checkbox", attribute "role" "switch", disabled True ] []
                        , text "User must change password at next logon"
                        ]
                    , input [ type_ "search", placeholder "Search", onInput SetSearch ] []
                    , span [ attribute "role" "button", onClick (OpenModal True) ] [ text "Open Modal" ]
                    , span
                        [ attribute "role" "button"
                        , onClick ChooseFile
                        , class "secondary"
                        , css [ m_4 ]
                        ]
                        [ text "Choose File" ]
                    , label [] [ text ("Range slider " ++ String.fromFloat model.rangeVal) ]
                    , range SetRangeVal 0 100 1 model.rangeVal
                    , fieldset []
                        [ legend [] [ text "wassup" ]
                        , label []
                            [ text "Date"
                            , input [ type_ "date", onInput PrintStr ] []
                            ]
                        , label []
                            [ text "Time"
                            , input [ type_ "time", onInput PrintStr ] []
                            ]
                        , label []
                            [ text "Color"
                            , input [ type_ "color", value model.color, onInput SetColor ] []
                            ]
                        ]
                    , div []
                        [ {- With checkboxes -}
                          details [ attribute "role" "list" ]
                            [ summary [ attribute "aria-haspopup" "listbox" ] [ text "Dropdown" ]
                            , ul [ attribute "role" "listbox" ]
                                [ li []
                                    [ label []
                                        [ input [ type_ "checkbox" ] []
                                        , text "Banana"
                                        ]
                                    ]
                                , li []
                                    [ label []
                                        [ input [ type_ "checkbox" ] []
                                        , text "Watermelon"
                                        ]
                                    ]
                                , li []
                                    [ label []
                                        [ input [ type_ "checkbox" ] []
                                        , text "Apple"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]

                {- Modal -}
                , node "dialog"
                    [ if model.modalOpen then
                        attribute "open" ""

                      else
                        class ""
                    , onClick (OpenModal False)
                    ]
                    [ article [ onClickStop NoOp ]
                        [ header []
                            [ span
                                [ attribute "aria-label" "Close"
                                , class "close"
                                , onClick (OpenModal False)
                                ]
                                []
                            , text "Modal title"
                            ]
                        , p []
                            [ text "Nunc nec ligula a tortor sollicitudin dictum in vel enim. Quisque facilisis turpis vel eros dictum aliquam et nec turpis. Sed eleifend a dui nec ullamcorper. Praesent vehicula lacus ac justo accumsan ullamcorper." ]
                        , footer []
                            [ span [ attribute "role" "button", class "secondary" ] [ text "cancel" ]
                            , span [ attribute "role" "button" ] [ text "confirm" ]
                            ]
                        ]
                    ]
                ]
        ]
    }


range : (Float -> msg) -> Float -> Float -> Float -> Float -> Html msg
range toMsg minVal maxVal stepVal val =
    input
        [ type_ "range"
        , value (String.fromFloat val)
        , step (String.fromFloat stepVal)
        , onInput (String.toFloat >> Maybe.withDefault ((minVal + maxVal) / 2) >> toMsg)
        , Attr.min (String.fromFloat minVal)
        , Attr.max (String.fromFloat maxVal)
        ]
        []


radioGroup : String -> (a -> msg) -> a -> List ( a, String ) -> Html msg
radioGroup title toMsg modelVar options =
    fieldset []
        (legend []
            [ text title ]
            :: List.map
                (\( opt, optStr ) ->
                    label []
                        [ input
                            [ type_ "radio"
                            , onCheck (\_ -> toMsg opt)
                            , checked (modelVar == opt)
                            ]
                            []
                        , text optStr
                        ]
                )
                options
        )


checkboxDropdown : (a -> Bool -> msg) -> List a -> List ( a, String ) -> Html msg
checkboxDropdown toMsg modelVar options =
    details [ attribute "role" "list" ]
        [ summary
            [ attribute "aria-haspopup" "listbox"
            , attribute "role" "button"
            , class "secondary"
            ]
            [ text
                (if List.isEmpty modelVar then
                    "Select..."

                 else
                    String.join ", "
                        (options
                            |> List.filterMap
                                (\( opt, optStr ) ->
                                    if List.member opt modelVar then
                                        Just optStr

                                    else
                                        Nothing
                                )
                        )
                )
            ]
        , ul [ attribute "role" "listbox" ]
            (List.map
                (\( opt, optStr ) ->
                    li []
                        [ label []
                            [ input
                                [ type_ "checkbox"
                                , onCheck (toMsg opt)
                                , checked (List.member opt modelVar)
                                ]
                                []
                            , text optStr
                            ]
                        ]
                )
                options
            )
        ]


dropdown : String -> (Maybe a -> msg) -> a -> List ( a, String ) -> Html msg
dropdown title toMsg modelVar options =
    div []
        [ label [] [ text title ]
        , select
            [ onInput
                (\strVal ->
                    options
                        |> List.filterMap
                            (\( opt, optStr ) ->
                                if optStr == strVal then
                                    Just opt

                                else
                                    Nothing
                            )
                        |> List.head
                        |> toMsg
                )
            ]
            (List.map
                (\( opt, optStr ) ->
                    option [ selected (modelVar == opt) ]
                        [ text optStr ]
                )
                options
            )
        ]


type Size
    = Small
    | Medium
    | Large
    | XLarge


sizes : List ( Size, String )
sizes =
    [ ( Small, "Small" )
    , ( Medium, "Medium" )
    , ( Large, "Large" )
    , ( XLarge, "X-Large" )
    ]


onClickStop : a -> Attribute a
onClickStop msg =
    stopPropagationOn "click" (De.map (\m -> ( m, True )) (De.succeed msg))


main : Program De.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        []
