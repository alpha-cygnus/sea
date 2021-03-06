port module Main exposing (..)

import Browser
import Browser.Events as BE
import Html exposing (Html, text, div, h1, img, span)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onMouseDown)
import Svg exposing (svg, path, rect, circle, g)
import Svg.Attributes exposing (x, y, cx, cy, r, stroke, strokeWidth, fill, transform, width, height, d)
import Array exposing (..)
import String exposing (fromFloat)
import Json.Decode as D
import Html.Events.Extra.Mouse as Mouse
import Pos
import Menu

port jslog : String -> Cmd msg

port fromjs : (String -> msg) -> Sub msg


-- settings
nodeSize = 30
portSize = 0.2



-- utils
ds cmd x = cmd ++ (fromFloat x)
ds2 cmd x y = cmd ++ (fromFloat x) ++ " " ++ (fromFloat y)
ds1x2 cmd (x, y) = cmd ++ (fromFloat x) ++ " " ++ (fromFloat y)
ds6 cmd a b c d e f = cmd ++ ([f, e, d, c, b, a] |> List.map fromFloat |> List.intersperse " " |> List.foldl (++) "")
ds3x2 cmd (a, b) (c, d) (e, f) = cmd ++ ([f, e, d, c, b, a] |> List.map fromFloat |> List.intersperse " " |> List.foldl (++) "")

dm = ds1x2 "m"
dM = ds1x2 "M"
dl = ds1x2 "l"
dL = ds1x2 "L"
dC = ds3x2 "C"
dc = ds3x2 "c"

pathd : List (Svg.Attribute Msg) -> List String -> Svg.Svg Msg
pathd attrs dd = path ((dd |> List.intersperse " " |> List.foldr (++) "" |> d) :: attrs ) []

translate (x, y) = " translate(" ++ (fromFloat x) ++ "," ++ (fromFloat y) ++ ")"
scale s = " scale(" ++ (fromFloat s) ++ ")"
scale2 (sx, sy) = " scale(" ++ (fromFloat sx) ++ "," ++ (fromFloat sy) ++ ")"

evtPos = .pagePos

createMovePicked : Picked -> Mouse.Event -> Msg
createMovePicked picked evt =
    let
        {selected, wasAt, pickedAt} = picked
        pos = evtPos evt
    in
        case selected of
            SelectedNode nodeId -> MoveNode nodeId <| Pos.add wasAt <| Pos.mul2 (1, -1) <| Pos.diff pos pickedAt
            SelectedPort nodeId _ -> MoveNode nodeId <| Pos.add wasAt <| Pos.mul2 (1, -1) <| Pos.diff pos pickedAt
            SelectedSchema -> NoOp


--listFind pred list = List.foldl (\elem res -> if pred elem then Just elem else res ) Nothing list

listFind pred list = case list of
    elem :: tail -> if pred elem then Just elem else listFind pred tail
    [] -> Nothing


fetchNode nodeId = .nodes >> listFind (\{id} -> id == nodeId)

fetchPort : (NodeId, PortName) -> Model -> Maybe (Node, PortDef)
fetchPort (nodeId, pname) m =
    case fetchNode nodeId m of
        Just node -> getPorts node.typ |> listFind (\{name} -> name == pname) |> Maybe.map (\p -> (node, p))
        Nothing -> Nothing

portCoords ({at}, {off, norm}) =
    let
        pos = Pos.add at <| Pos.mul nodeSize <| Pos.add off <| Pos.mul portSize norm
    in
        (pos, Pos.add pos <| Pos.mul (nodeSize*3) norm)

---- MODEL ----
type LinkType = Audio

type OscType = Sine | Saw | Rect

type FilterType = LowPass | HighPass | BandPass

type NodeType
    = Osc { typ: OscType }
    | Const { value: Float }
    | Filter { typ: FilterType, q: Float }

type PortDir = Input | Output

type alias PortDef = {name: String, off: (Float, Float), norm: (Float, Float), dir: PortDir, typ: LinkType}

input name = {name = name, off = (-1, 0), norm = (-1, 0), dir = Input, typ = Audio}
output name = {name = name, off = (1, 0), norm = (1, 0), dir = Output, typ = Audio}
withOff pos pdef = {pdef | off = pos, norm = pos}
withNorm pos pdef = {pdef | norm = pos}

getPorts : NodeType -> List PortDef
getPorts nt =
    case nt of
        Osc _ ->
            [ output "out"
            , input "freq" |> withOff(0, -1)
            , input "detune" |> withOff(0, 1)
            ]
        Const _ -> 
            [ output "out"
            ]
        Filter _ ->
            [ output "out"
            , input "freq" |> withOff(0, -1)
            , input "detune" |> withOff(0, 1)
            , input "inp"
            ]

type alias Node =
    {
        id: NodeId,
        at: (Float, Float),
        typ: NodeType
    }

type alias NodeId = Int

type alias PortName = String

type alias Link =
    {
        from: (NodeId, PortName),
        to: (NodeId, PortName),
        typ: LinkType
    }

type Selection
    = SelectedNode NodeId
    | SelectedPort NodeId PortName
    | SelectedSchema

type alias Picked = {selected: Selection, wasAt: (Float, Float), pickedAt: (Float, Float) }

type alias Model =
    {
        str: String,
        nodes: List Node,
        links: List Link,
        picked: Maybe Picked,
        selected: Maybe Selection,
        menu: Maybe (Menu.Model Msg)
    }


-- init
init : ( Model, Cmd Msg )
init =
    ( {
        str = "nothing",
        picked = Nothing,
        nodes =
            [ { id = 1, at = (-100, 0), typ = Osc { typ = Sine } }
            , { id = 2, at = ( 100, 0), typ = Osc { typ = Sine } }
            ],
        links =
            [ { from = (1, "out"), to = (2, "freq"), typ = Audio}
            ],
        selected = Nothing,
        menu = Nothing
    }, Cmd.none )



---- UPDATE ----
type Msg
    = NoOp
    | TestJslog String
    | FromJS String
    | PickNode Node (Float, Float)
    | PickPort Node PortName (Float, Float)
    | MoveNode NodeId (Float, Float)
    | Select Selection
    | SelectNode NodeId
    | SelectPort (NodeId, PortName)
    | ShowMenu Selection Pos.Pos
    | Unselect
    | Drop

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    TestJslog str -> (model, jslog str)
    FromJS str -> ({model | str = str}, Cmd.none)
    PickNode {id, at} pos -> ({model | picked = Just {selected = SelectedNode id, wasAt = at, pickedAt = pos }}, Cmd.none)
    PickPort {id, at} pn pos -> ({model | picked = Just {selected = SelectedPort id pn, wasAt = at, pickedAt = pos }}, Cmd.none)
    Drop -> ({model | picked = Nothing }, Cmd.none)
    MoveNode nodeId to -> ({model | nodes = List.map (\node -> if node.id == nodeId then {node | at = to} else node) model.nodes}, Cmd.none)
    Select sel -> ({model | selected = Just sel, picked = Nothing, menu = Nothing}, Cmd.none)
    SelectNode mn -> ({model | selected = Just (SelectedNode mn), picked = Nothing, menu = Nothing}, Cmd.none)
    SelectPort (nodeId, pn) -> ({model | selected = Just (SelectedPort nodeId pn), picked = Nothing, menu = Nothing}, Cmd.none)
    Unselect -> ({model | picked = Nothing, selected = Nothing, menu = Nothing}, Cmd.none)
    ShowMenu sel pos -> ({model | menu = Just <| Menu.init pos <| getMenuItems model sel}, Cmd.none)
    NoOp -> (model, Cmd.none)



-- subs
pickedSubs model =
    case model.picked of
        Just picked ->
            [
                BE.onMouseUp <| D.map (\e -> if (picked.pickedAt == evtPos e) then Select picked.selected else Drop) Mouse.eventDecoder,
                BE.onMouseMove <| D.map (createMovePicked picked) Mouse.eventDecoder
            ]
        Nothing -> []
interOpSubs model =
    [
        fromjs FromJS
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
    pickedSubs model ++
    interOpSubs model |>
    Sub.batch



-- render
renderNodeType : RenderFlags -> NodeType -> List (Svg.Svg msg)
renderNodeType flags node =
    let
        sk = if flags.picked then "red" else "black"
        sw = if flags.selected then "0.1" else "0.05"
    in case node of
        _ -> [circle [cx "0", cy "0", r "1", stroke sk, strokeWidth sw, fill "white"] []]

renderPort m node {name, off, norm, dir, typ} =
    let
        (ox, oy) = Pos.add off <| Pos.mul 0.2 norm
        nodeId = node.id
        selected = case m.selected of
            Just (SelectedPort n p) -> nodeId == n && name == p
            _ -> False
        fl = case dir of
            Input -> "green"
            Output -> "red"
    in
        g 
            [ Mouse.onDown <| \e -> if e.button == Mouse.MainButton then PickPort node name (evtPos e) else NoOp
            , Mouse.onContextMenu <| \e -> ShowMenu (SelectedPort node.id name) (evtPos e)
            ]
            [circle [cx <| fromFloat ox, cy <| fromFloat oy, r "0.2", stroke "black", strokeWidth <| if selected then "0.1" else "0.05", fill fl] []]
    

type alias RenderFlags =
    {
        picked: Bool,
        selected: Bool
    }
emptyFlags : RenderFlags
emptyFlags = {picked = False, selected = False}

renderNode : Model -> Node -> Svg.Svg Msg
renderNode m node =
    let
        {id, at, typ} = node
        isPicked = case m.picked of
            Just {selected} -> case selected of
                SelectedNode nodeId -> id == nodeId
                _ -> False
            _ -> False
        isSelected = case m.selected of
            Just (SelectedNode nodeId) -> id == nodeId
            _ -> False
        flags = {emptyFlags | picked = isPicked, selected = isSelected }
    in
        g
            [ transform <| translate at ++ scale 30
            ]
            <| [g
                    [ Mouse.onDown <| \e -> if e.button == Mouse.MainButton then PickNode node (evtPos e) else NoOp
                    , Mouse.onContextMenu <| \e -> ShowMenu (SelectedNode node.id) (evtPos e)
                    ] <| renderNodeType flags typ]
            ++ List.map (renderPort m node) (getPorts typ)

renderLink : Model -> Link -> List (Svg.Svg Msg)
renderLink m {from, to, typ} =
    let 
        (f, t) = (fetchPort from m, fetchPort to m)
    in case (f, t) of
        (Just ff, Just tt) ->
            let
                ((fxy, fn), (txy, tn)) = (portCoords ff, portCoords tt)
            in
                [ pathd [stroke "black", fill "none"] [dM fxy, dC fn tn txy]
                ]
        _ -> []

getMenuItems : Model -> Selection -> List (Menu.Item Msg)
getMenuItems m sel =
    case sel of
        SelectedNode nodeId ->
            [ {caption = "Edit", msg = Unselect}
            , {caption = "Delete", msg = Unselect}
            ]
        SelectedPort _ _ ->
            [ {caption = "Disconnect all", msg = Unselect}
            ]
        SelectedSchema ->
            [ {caption = "New Osc", msg = Unselect}
            ]

---- VIEW ----
view : Model -> Html Msg
view model =
    div [] <|
        [ svg [width "800px", height "800px"]
            [ rect [x "0", y "0", width "800", height "800", fill "white", stroke "black"
            , onClick Unselect
            , Mouse.onContextMenu <| \e -> ShowMenu (SelectedSchema) (evtPos e)
            ] []
            , g [transform <| translate(400, 400) ++ scale2(1, -1)]
                [ List.map (renderLink model) model.links |> List.foldl (++) [] |> g []
                , List.map (renderNode model) model.nodes |> g []
                ]
            ]
        ] ++ (Maybe.withDefault [] <| Maybe.map Menu.view model.menu)



---- PROGRAM ----
main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
