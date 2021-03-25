module FSA exposing (main)

import Browser
import Browser.Events
import Browser.Dom as Dom
import Task
import Html exposing (Html)
import Random exposing (Generator)
import Time exposing (Posix)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Debug
import Collage exposing (..)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Collage.Text exposing (..)
import Collage.Events exposing (..)
import Color
import Html.Attributes as Attr
import Html.Events exposing (onInput, onClick)
import SingleSlider exposing (..)
import Bootstrap.CDN as CDNs
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import StackAndQueue exposing (..)
import StateMachines as SM exposing (..)
import File exposing (File)
import File.Download as Download
import File.Select as Select


----------------------------------------------------------------------

-- Initializes in the browser
main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Type for the model includes:
  -- currentFA : FiniteAutomata - The current automata
  -- currentWord : String - The string which the automata is reading
  -- currentSelect (Edge, State)? - The current edge or state the user has selected
  -- running : Bool - Whether the automata is running or paused
  -- runSpeedSlider : SingleSlider Msg - Manages the running speed slider
  -- currentSpeed : Float - The current speed as a float
  -- elapsedTime : Float - The elapsed time
  -- backWord : String - Stores the remainder of a word while it's entirety is being read by a FA 
  -- accepted : Bool? - Whether the last word read was accepted by the FA
  -- error : String? - String used to give the user error messages about their FA construction
  -- lastElapse : Bool - Boolean used to help manage time elapsing
  -- faType : Sting - What type of finite automata is in use. Either DFA, NFA, PDA, or QA
  -- reString : String - String for renaming
  -- focused : Bool - Used for when the user has focused on a textbox
  -- mousePoint : Point - The location of the mouse
  -- movingSelected : Bool - Boolean for whether or not the user is dragging their selected node/edge
  -- shiftPressed : Bool - Is shift pressed
  -- drawingEdge : Bool - Whether the user is in the process of drawing an edge
  -- reStringSelected : Bool - Whether the selected edge/node will be restrung
  -- lastWord : String - Last word read
  -- dfaStore : FiniteAutomata - DFA which is currently in progress. Similar for NFA, PDA, QA
  -- showError : Bool - Should show error modal
  -- showErrorMsg : Bool - Should show error as message
  -- controlScreen : Int - Which page of the control screen we're viewing
  -- bulkWords : String - Allows the simultaneous reading of many words
  -- saveFile : String - target file for saving an automata
  -- modalVisibility : Modal.Visibility - Used to control modal visibility
  -- modalContent : String - Used to control text for a modal
type alias Model =
  { currentFA : FiniteAutomata
  , currentWord : String
  , currentSelect : Maybe (Either State Edge)
  , running : Bool
  , runSpeedSlider : SingleSlider.SingleSlider Msg
  , currentSpeed : Float
  , elapsedTime : Float
  , backWord : String
  , accepted : Maybe Bool
  , error : Maybe String
  , lastElapse : Bool
  , faType : String
  , reString : String
  , focused : Bool
  , mousePoint : SM.Point
  , movingSelected : Bool
  , shiftPressed : Bool
  , drawingEdge : Bool
  , reStringSelected : Bool
  , lastWord : String
  , lastBackWord : String
  , dfaStore : FiniteAutomata
  , nfaStore : FiniteAutomata
  , pdaStore : FiniteAutomata
  , qaStore : FiniteAutomata
  , showError : Bool
  , showErrorMsg : String
  , controlScreen : Int
  , bulkWords : String
  , bulkResults : List (String, Bool)
  , saveFile : String
  , modalVisibility : Modal.Visibility
  , modalContent : String
  }

-- Messages which can be sent to the controller
type Msg = SelectState SM.Point Collage.Point
  | DropSelection Collage.Point
  | SelectEdge Int Int Collage.Point
  | SelectNone
  | AddState
  | AddEdge Int Int
  | UpdateSpeed
  | RemoveSelect
  | ReStringSelect
  | ToggleInitSelect
  | ToggleFinalSelect
  | RunWord
  | RunBulk
  | ChangeWord String
  | ChangeBulk String
  | ChangeString String
  | ChangeSaveFile String
  | SpeedSliderChange Float
  | Tick
  | OnFocus
  | OffFocus
  | UpdateMousePos Collage.Point
  | ShiftPressed
  | ShiftReleased
  | OnFocusReString
  | Noop
  | DefaultDrop
  | SwitchDFA
  | SwitchNFA
  | SwitchPDA
  | SwitchQA
  | ClearBoard
  | NextControl
  | PrevControl
  | Save
  | JSONFARequested
  | JSONFALoaded File
  | JSONToFA String
  | CloseModal
  | MinimizeMN


type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, Cmd.none)

-- Initalizes the model
initModel : Model
initModel =
  { currentFA = initFA
  , dfaStore = initFA
  , nfaStore = initFA
  , pdaStore = initFA
  , qaStore = initFA
  , currentWord = ""
  , currentSelect = Nothing
  , running = False
  , runSpeedSlider =
    SingleSlider.init
        { min = 0
        , max = 10
        , value = 0
        , step = 1
        , onChange = SpeedSliderChange
        }
  , currentSpeed = 0
  , elapsedTime = 0
  , backWord = ""
  , accepted = Nothing
  , error = Nothing
  , lastElapse = False
  , faType = "DFA"
  , reString = ""
  , focused = False
  , mousePoint = {x = 0, y = 0}
  , movingSelected = False
  , shiftPressed = False
  , drawingEdge = False
  , reStringSelected = False
  , lastWord = ""
  , lastBackWord = ""
  , showError = False
  , showErrorMsg = ""
  , controlScreen = 0
  , bulkWords = ""
  , bulkResults = []
  , saveFile = ""
  , modalVisibility = Modal.shown
  , modalContent = "instructions"
  }

-- Decodes key presses
keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

-- Manages the input box focusing
focusInputBox : Cmd Msg
focusInputBox =
    Task.attempt (\_ -> Noop) (Dom.focus "edgeTransInput")

-- Manages the input box unfocusing
unFocusInputBox : Cmd Msg
unFocusInputBox =
    Task.attempt (\_ -> Noop) (Dom.blur "edgeTransInput")

-- Manages browser events like keypresses
subscriptions : Model -> Sub Msg
subscriptions model =
  let
    posixToTick : Posix -> Msg
    posixToTick p = Tick
  in
    Sub.batch
        [ Browser.Events.onAnimationFrame (posixToTick),
          Browser.Events.onMouseUp (Decode.succeed DefaultDrop),
          Browser.Events.onKeyDown
            (Decode.map (\key -> if key == "Shift"
              then ShiftPressed
              else if not model.focused then (if key == "i" then ToggleInitSelect
                else if key == "f" then ToggleFinalSelect
                else if key == "ArrowRight" then NextControl
                else if key == "ArrowLeft" then PrevControl
                else if key == "Backspace" then RemoveSelect
                else Noop) else if key == "Enter" && model.reStringSelected then
                  ReStringSelect
                  else  Noop) keyDecoder),
          Browser.Events.onKeyUp
            (Decode.map (\key -> if key == "Shift" then ShiftReleased
                else Noop) keyDecoder)
        ]
        
-- Default update to the model to reset to normal
getSwitchDefault : Model -> Model
getSwitchDefault model =
  {model | currentSelect = Nothing, running = False,
    currentWord = model.backWord, accepted = Nothing, lastWord = "",
    showError = False}

-- Hangles updates to the model from the various messages
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnFocus -> ({model | focused = True}, Cmd.none)
    OnFocusReString -> ({model | focused = True, reStringSelected = True}, Cmd.none)
    OffFocus -> ({model | focused = False, reStringSelected = False}, Cmd.none)
    ShiftPressed -> ({model | shiftPressed = True, drawingEdge = True}, Cmd.none)
    ShiftReleased -> ({model | shiftPressed = False, drawingEdge = False}, Cmd.none)
    ClearBoard -> ({model | running = False, currentSelect = Nothing, currentFA = initFA,
      currentWord = model.backWord, accepted = Nothing, showError = False}, Cmd.none)
    NextControl -> ({model | controlScreen = min (model.controlScreen + 1) 2 }, Cmd.none)
    PrevControl -> ({model | controlScreen = max (model.controlScreen - 1) 0 }, Cmd.none)
    -- Switching between DFAs, NFAs, PDAs, and QAs. Which I could condense this more
    SwitchDFA -> if model.faType == "DFA" then
        (model, Cmd.none)
        else
          let
            def = getSwitchDefault model
          in
            if model.faType == "NFA" then ({ def | currentFA = model.dfaStore, nfaStore = model.currentFA, faType = "DFA"}, Cmd.none)
              else if model.faType == "PDA" then ({ def | currentFA = model.dfaStore, pdaStore = model.currentFA, faType = "DFA"}, Cmd.none)
              else ({ def | currentFA = model.dfaStore, qaStore = model.currentFA, faType = "DFA"}, Cmd.none)
    SwitchNFA -> if model.faType == "NFA" then
        (model, Cmd.none)
        else
          let
            def = getSwitchDefault model
          in
            if model.faType == "DFA" then ({ def | currentFA = model.nfaStore, dfaStore = model.currentFA, faType = "NFA"}, Cmd.none)
              else if model.faType == "PDA" then ({ def | currentFA = model.nfaStore, pdaStore = model.currentFA, faType = "NFA"}, Cmd.none)
              else ({ def | currentFA = model.nfaStore, qaStore = model.currentFA, faType = "NFA"}, Cmd.none)
    SwitchPDA -> if model.faType == "PDA" then
        (model, Cmd.none)
        else
          let
            def = getSwitchDefault model
          in
            if model.faType == "DFA" then ({ def | currentFA = model.pdaStore, dfaStore = model.currentFA, faType = "PDA"}, Cmd.none)
              else if model.faType == "NFA" then ({ def | currentFA = model.pdaStore, nfaStore = model.currentFA, faType = "PDA"}, Cmd.none)
              else ({ def | currentFA = model.pdaStore, qaStore = model.currentFA, faType = "PDA"}, Cmd.none)
    SwitchQA -> if model.faType == "QA" then
        (model, Cmd.none)
        else
          let
            def = getSwitchDefault model
          in
            if model.faType == "DFA" then ({ def | currentFA = model.qaStore, dfaStore = model.currentFA, faType = "QA"}, Cmd.none)
              else if model.faType == "NFA" then ({ def | currentFA = model.qaStore, nfaStore = model.currentFA, faType = "QA"}, Cmd.none)
              else ({ def | currentFA = model.qaStore, pdaStore = model.currentFA, faType = "QA"}, Cmd.none)
    -- Handles mouse updates
    UpdateMousePos p ->
      let
        reg = {model | mousePoint =
          {x = max (min (((Tuple.first p) - 23) / 800) 0.983) 0.017,
          y = max (min (1 - ((Tuple.second p) - 50) / 550) 0.983) 0.017}}
      in
        case model.currentSelect of
          Nothing -> (reg, Cmd.none)
          Just (Right _) -> (reg, Cmd.none)
          Just (Left s) -> if not model.movingSelected || model.drawingEdge then
            (reg, Cmd.none)
            else
              case s of
                (N name _ oldP) -> ({reg | currentFA = changeStateData model.currentFA s name reg.mousePoint,
                  currentSelect = Just (Left (changeStateDataIndividual s name reg.mousePoint)) }, Cmd.none)
    -- Allows user to add states
    AddState -> let
                  (defaultName, iden) = case model.currentFA of
                    (FA _ _ _ _ _ nextID) -> ("s" ++ String.fromInt nextID, nextID)
                  mp = model.mousePoint
                in
                  ({model | currentFA = addState defaultName mp model.currentFA,
                  currentSelect = Just (Left (N defaultName iden mp))}, Cmd.none)
    -- Allows user to select nodes
    SelectState point _ ->
      let
        resS = case model.currentFA of
          (FA states _ _ _ _ _) -> getStateFromPos states point
        res = case resS of
          Nothing -> Nothing
          Just s -> Just (Left s)
        resName = case resS of
          Nothing -> ""
          Just (N name _ _) -> name
        regChanges = { model | currentSelect = res, reString = resName, movingSelected = True}
      in
        if model.shiftPressed then
          ({regChanges | drawingEdge = True}, Cmd.none )
          else (regChanges, Cmd.none )
    -- Allows user to drag and drop nodes
    DefaultDrop -> ( {model | drawingEdge = False, movingSelected = False}, Cmd.none )
    DropSelection point ->
      case model.currentSelect of
        Nothing -> (model, Cmd.none)
        Just (Right _) -> (model, Cmd.none)
        Just (Left s) -> if model.movingSelected then
            case s of
              (N name iden _) ->
                if (model.drawingEdge && model.shiftPressed) then
                  let
                    (ss, es) = case model.currentFA of
                      (FA ss_ es_ _ _ _ _) -> (ss_, es_)
                  in
                    case getStateFromApproxPos ss model.mousePoint of
                      Nothing -> ({model | drawingEdge = False, movingSelected = False}, Cmd.none)
                      Just (N _ toIden _) -> case getEdgeFromFT es iden toIden of
                        Nothing -> if model.faType == "PDA" || model.faType == "QA" then
                              ({model | currentFA = addEdge iden toIden "//" model.currentFA,
                                currentSelect = Just (Right (E (iden, toIden) "//")),
                                reString = "//"}, focusInputBox)
                            else
                              ({model | currentFA = addEdge iden toIden "" model.currentFA,
                                currentSelect = Just (Right (E (iden, toIden) "")),
                                reString = ""}, focusInputBox)
                        Just (E _ t) -> ({model | currentSelect = Just (Right (E (iden, toIden) t)),
                          reString = t}, focusInputBox)
                  else ({ model | currentFA = changeStateData model.currentFA s name model.mousePoint,
                      currentSelect = Just (Left (changeStateDataIndividual s name model.mousePoint)), movingSelected = False}, Cmd.none)
            else ({model | drawingEdge = False }, Cmd.none)
    -- Allows user to select edges
    SelectEdge from to _ ->
      let
        resE = case model.currentFA of
          (FA _ edges _ _ _ _) -> getEdgeFromFT edges from to
        res = case resE of
          Nothing -> Nothing
          Just e -> Just (Right e)
        resTrans = case resE of
          Nothing -> ""
          Just (E _ trans) -> trans
      in
        ({ model | currentSelect = res, reString = resTrans }, focusInputBox )
    -- Allows user to clear their selection by clicking on nothing
    SelectNone -> ({ model | currentSelect = Nothing, showError = False }, Cmd.none )
    -- Allows user to remove nodes and edges
    RemoveSelect -> case model.currentSelect of
      Nothing -> (model, Cmd.none)
      Just (Left s) -> ({ model | currentFA = removeState s model.currentFA, currentSelect = Nothing }, Cmd.none)
      Just (Right e) -> ({ model | currentFA = removeEdge e model.currentFA, currentSelect = Nothing }, Cmd.none)
    ReStringSelect -> case model.currentSelect of
      Nothing -> (model, Cmd.none)
      Just (Right e) ->
        case e of
          (E (from, to) _) ->
            let
              (ss, es) = case model.currentFA of
                (FA ss_ es_ _ _ _ _) -> (ss_, es_)
              fromState = unwrap (getStateFromID ss from)
              adjacentEdges = List.filter (\singleE -> (isEdgeFromState fromState singleE) && (e /= singleE)) es
              uniqueChars = String.fromList (stripDuplicates (String.toList model.reString))
              getPossible ucs = case ucs of
                [] -> True
                c :: restUcs ->
                  (not (List.member c (sumTrans adjacentEdges))) && (getPossible restUcs)
              possible = getPossible (String.toList uniqueChars) 
            in
              if possible || model.faType /= "DFA" then
                if model.faType == "NFA" || model.faType == "DFA" then
                  ({ model | currentFA = changeEdgeData model.currentFA e uniqueChars,
                    currentSelect = Just (Right (changeEdgeDataIndividual e uniqueChars)),
                    reString = uniqueChars, showError = False}, unFocusInputBox)
                  else if (possibleParseTransChars model.reString) then
                    ({ model | currentFA = changeEdgeData model.currentFA e model.reString,
                    currentSelect = Just (Right (changeEdgeDataIndividual e model.reString)),
                    showError = False}, unFocusInputBox)
                  else
                    ({ model | showError = True, showErrorMsg = "Error: PDA and QA transitions must have the format [One or zero characters]/[String]/[String] separated by comma. Whitespace ignored."}, Cmd.none)
                else ({ model | reString = uniqueChars, showError = True, showErrorMsg = "Error: Cannot have two transitions from the same state for the same character in a DFA"}, Cmd.none)
                  
      Just (Left s) -> case s of
        (N _ _ p) -> ({ model | currentFA = changeStateData model.currentFA s model.reString p,
          currentSelect = Just (Left (changeStateDataIndividual s model.reString p))}, unFocusInputBox)
    ToggleInitSelect -> case model.currentSelect of
      Nothing -> (model, Cmd.none)
      Just (Right _) -> (model, Cmd.none)
      Just (Left s) -> case isStateInitial model.currentFA s of
        True -> if model.faType == "DFA" then
          ({ model | currentFA = removeInitialStateDFA model.currentFA }, Cmd.none)
          else ({ model | currentFA = removeInitialStateNFA s model.currentFA }, Cmd.none)
        False -> if model.faType == "DFA" then
          ({ model | currentFA = setInitialStateDFA s model.currentFA }, Cmd.none)
          else ({ model | currentFA = addInitialStateNFA s model.currentFA }, Cmd.none)
    ToggleFinalSelect -> case model.currentSelect of
      Nothing -> (model, Cmd.none)
      Just (Right _) -> (model, Cmd.none)
      Just (Left s) -> case isStateFinal model.currentFA s of
        True -> ({ model | currentFA = removeFinal s model.currentFA }, Cmd.none)
        False -> ({ model | currentFA = addFinal s model.currentFA }, Cmd.none)
    ChangeWord newWord -> if model.running then ({ model | backWord = newWord }, Cmd.none )
        else ({ model | currentWord = newWord, backWord = newWord }, Cmd.none )
    ChangeBulk newBulk -> ({ model | bulkWords = newBulk }, Cmd.none )
    ChangeString newString ->  ({ model | reString = newString }, Cmd.none )
    ChangeSaveFile newSave -> ({ model | saveFile = newSave }, Cmd.none )
    -- Allows user to adjust speed of the automata visualization
    SpeedSliderChange val ->
            let
                newSlider =
                    SingleSlider.update val model.runSpeedSlider
            in
            ( { model | runSpeedSlider = newSlider, currentSpeed = val }, Cmd.none )
    Tick ->
      if model.running then
        if model.elapsedTime > 300 then
          let
            updateType = if model.faType == "DFA" then updateDFA
              else if model.faType == "NFA" then updateNFA
              else if model.faType == "PDA" then updatePDA
              else updateQA
            (wrapped_newWord, wrapped_newFA) = updateType (model.currentWord) (model.currentFA)
            newFA = unwrap wrapped_newFA
            standUp = { model | elapsedTime = 0, currentFA = newFA }
          in
            case wrapped_newWord of
              Nothing -> case model.lastElapse of
                False -> ({standUp | lastElapse = True }, Cmd.none ) -- currentWord = ""
                True -> ({ model | elapsedTime = 0, currentFA = clearCurrent model.currentFA,
                    currentWord = model.backWord, accepted = Just (accept wrapped_newFA), running = False,
                    lastElapse = False, lastWord = model.lastBackWord }, Cmd.none )
              Just w -> ({standUp | currentWord = w }, Cmd.none )
          else
            if model.currentSpeed == 0 then
              ({ model | elapsedTime = (model.elapsedTime + 40) }, Cmd.none )
              else ({ model | elapsedTime = (model.elapsedTime + ((-2 * model.currentSpeed) + 20)) }, Cmd.none )
        else (model, Cmd.none)
    -- Runs a word through the automaton at the selected speed
    RunWord -> if model.running then ({model | running = False, currentFA = clearCurrent model.currentFA,
          currentWord = model.backWord, accepted = Nothing}, Cmd.none)
        else if model.faType == "DFA" then
            ({ model | running = True, currentFA = startRunDFA (model.currentFA), accepted = Nothing, lastBackWord = model.currentWord }, Cmd.none )
            else if model.faType == "NFA" then ({ model | running = True, currentFA = startRunNFA (model.currentFA), accepted = Nothing, lastBackWord = model.currentWord }, Cmd.none )
            else if model.faType == "PDA" then ({ model | running = True, currentFA = startRunPDA (model.currentFA), accepted = Nothing, lastBackWord = model.currentWord }, Cmd.none )
            else ({ model | running = True, currentFA = startRunQA (model.currentFA), accepted = Nothing, lastBackWord = model.currentWord }, Cmd.none )
    -- Instantly runs many words
    RunBulk -> if model.faType == "DFA" then 
      ({model | bulkResults = runBulkDFA (splitBulk model.bulkWords) model.currentFA}, Cmd.none)
      else if model.faType == "NFA" then
        ({model | bulkResults = runBulkNFA (splitBulk model.bulkWords) model.currentFA}, Cmd.none)
      else if model.faType == "PDA" then
        ({model | bulkResults = runBulkPDA (splitBulk model.bulkWords) model.currentFA}, Cmd.none)
      else ({model | bulkResults = runBulkQA (splitBulk model.bulkWords) model.currentFA}, Cmd.none)
    -- Saving and loading from JSON
    Save -> (model, downloadFA model.saveFile model.currentFA model.faType)
    JSONFARequested -> (model, requestFA)
    JSONFALoaded file -> (model, Task.perform JSONToFA (File.toString file))
    JSONToFA json_string ->
      let
        attempt = Result.toMaybe (Decode.decodeString decodeFA json_string)
      in
        case attempt of
          Just fa ->
            if fa.faType == model.faType then
              ({ model | currentFA = decodeFAToFA fa, currentSelect = Nothing, running = False }, Cmd.none)
            else
              ({model | modalVisibility = Modal.shown, modalContent = "errorUnconvertible" }, Cmd.none)
          Nothing -> ({model | modalVisibility = Modal.shown, modalContent = "loadFailure" }, Cmd.none)
    CloseModal -> ({ model | modalVisibility = Modal.hidden }, Cmd.none)
    MinimizeMN -> ({model | currentFA = minimizeMN model.currentFA, currentSelect = Nothing }, Cmd.none)
    _ -> (model, Cmd.none)

-- Visual representation of a FA's nodes
faToCircles : FiniteAutomata -> Maybe (Either State Edge) -> List (Collage Msg)
faToCircles (FA states _ initStates curStates finStates _) currentSelect =
  let
    makeName name = fromString (String.left 4 name)
      |> size normal
      |> color Color.black
      |> Collage.rendered
    circ s =
      let
        fillDefault = if List.member s (extractCurFinite curStates) then Color.green else Color.white
        fillColor = case currentSelect of
          Nothing -> fillDefault
          Just (Left currentS) -> if s == currentS then Color.lightBlue
            else fillDefault
          Just (Right _) -> fillDefault
        solidity = if List.member s initStates then dash else solid
        borderColor = if List.member s finStates then Color.red else Color.black
      in
        circle 16 |> styled (uniform fillColor, (solidity thick (uniform borderColor)))
  in
    List.map (\s -> case s of
      (N name _ point) -> (group [makeName name, circ s])
         |> shift ((point.x - 0.5) * 790
         , (point.y - 0.5) * 540)
         |> Collage.Events.onMouseDown (SelectState {x = point.x, y = point.y})
         |> Collage.Events.onMouseMove UpdateMousePos
         |> Collage.Events.onMouseUp DropSelection) states

-- Visual representation of a FA's edges
faToEdges : FiniteAutomata -> String -> Maybe (Either State Edge) -> String -> List (Collage Msg)
faToEdges (FA states edges _ curStates _ _) w currentSelect faType =
  List.map (\e -> case e of
    (E (from, to) transChars) ->
      let
        fromState = case getStateFromID states from of
          Nothing -> Debug.todo "Invalid from in edge"
          Just fromS -> fromS
        upNext = (List.member fromState (extractCurFinite curStates)) && (case String.toList w of
          [] -> False
          c :: _ -> if faType == "DFA" || faType == "NFA" then
              isCharInEdge c e
              else if faType == "PDA" then
                let 
                  validList = List.map (\(_, stack) -> List.length (getValidPDA c stack e) > 0) (toAllStack curStates)
                in
                  List.foldl (||) False validList
              else
                let 
                  validList = List.map (\(_, queue) -> List.length (getValidQA c queue e) > 0) (toAllQueue curStates)
                in
                  List.foldl (||) False validList)
        inEpsClosure = (List.member fromState (extractCurFinite curStates)) && String.length transChars == 0
        colDefault = if upNext || (if faType == "NFA" then inEpsClosure else False) then
          Color.green
          else Color.black 
        col = case currentSelect of
          Nothing -> colDefault
          Just (Left _) -> colDefault
          Just (Right currentE) -> 
            if e == currentE then Color.lightBlue
              else colDefault
        toPoint = case getStateFromID states to of
          Nothing -> Debug.todo "Invalid to in edge"
          Just (N _ _ toP) -> toP
        fromPoint = case fromState of
          (N _ _ fromP) -> fromP
        xMin = (unwrap (List.minimum [fromPoint.x, toPoint.x])) - 0.5
        xMax = (unwrap (List.maximum [fromPoint.x, toPoint.x])) - 0.5
        xAverage = (xMin + xMax) / 2
        yMin = (unwrap (List.minimum [fromPoint.y, toPoint.y])) - 0.5
        yMax = (unwrap (List.maximum [fromPoint.y, toPoint.y])) - 0.5
        fromPCen = (((fromPoint.x - 0.5) - xMin) * 790, ((fromPoint.y - 0.5) - yMin) * 540)
        fromXCen = Tuple.first fromPCen
        fromYCen = Tuple.second fromPCen
        toPCen = (((toPoint.x  - 0.5) - xMin) * 790, ((toPoint.y  - 0.5)  - yMin) * 540)
        toXCen = Tuple.first toPCen
        toYCen = Tuple.second toPCen
        xCenAverage = ((Tuple.first fromPCen) + (Tuple.first toPCen)) / 2
        yCenAverage = ((Tuple.second fromPCen) + (Tuple.second toPCen)) / 2
        angle = atan2 (toYCen - fromYCen) (toXCen - fromXCen)
        -- ang = Debug.todo (Debug.toString angle)
        arrowLine = if fromPCen == toPCen then
            let
              upRight = (fromXCen + 29, fromYCen + 50)
              upLeft = (fromXCen - 29, fromYCen + 50)
            in
              group [segment fromPCen upRight |> traced (solid thick (uniform col)),
                segment upRight upLeft |> traced (solid thick (uniform col)),
                segment upLeft fromPCen |> traced (solid thick (uniform col))]
            else segment fromPCen toPCen |> traced (solid thick (uniform col))
        trans = let
                  pos = if fromPCen == toPCen then
                    (xCenAverage, yCenAverage + 60)
                    else (xCenAverage + ((sin angle) * 15), yCenAverage - ((cos angle) * 15))
                  epsTrans = if transChars == "" && (faType == "NFA") then "\u{03F5}" else transChars
                in
                  fromString epsTrans
                  |> size normal
                  |> color col
                  |> Collage.rendered
                  |> shift pos
        arrowHead = if fromPCen == toPCen then
            triangle 20 |> filled (uniform col) |> rotate (degrees -30)
            |> shift (fromXCen - 14, fromYCen + 28)
            else triangle 20 |> filled (uniform col)
            |> rotate (angle - (pi / 2))
            |> shift (toXCen - ((cos angle) * 25), toYCen - ((sin angle) * 25))
        arrow = group([arrowHead, arrowLine])
        getShift = (xMin * 790, yMin * 540)
      in
        (group [trans, arrow])
        |> shift getShift
        |> Collage.Events.onMouseDown (SelectEdge from to)
        |> Collage.Events.onMouseMove UpdateMousePos
        |> Collage.Events.onMouseUp DropSelection) edges

-- Draws the screen and manages modals
view : Model -> Html Msg
view model =
  let
    getAlphabet = case model.currentFA of
      (FA _ es _ _ _ _) -> if model.faType == "DFA" || model.faType == "NFA" then
          "Input alphabet: \u{03A3} = " ++ (String.fromList (List.sort (sumTrans es)))
          else "Input alphabet: \u{03A3} = " ++ (String.fromList (List.sort (sumTransInputDS es)))
    getAlphabetDS = case model.currentFA of
      (FA _ es _ _ _ _) -> if model.faType == "DFA" || model.faType == "NFA" then
          ""
          else if model.faType == "PDA" then
            "Stack alphabet: \u{0393} = " ++ (String.fromList (sumTransDS es))
          else
            "Queue alphabet: \u{0393} = " ++ (String.fromList (sumTransDS es))
    center el = Html.div [Attr.class "lrWrapper"] [
                  Html.div [] [],
                  el,
                  Html.div [] []
                ]
    states = group (faToCircles model.currentFA model.currentSelect)
    edges = group (faToEdges model.currentFA model.currentWord model.currentSelect model.faType)
    rect =
        Collage.rectangle 800 550
            |> filled (uniform Color.white)
            |> Collage.Events.onClick SelectNone
            |> Collage.Events.onDoubleClick AddState
            |> Collage.Events.onMouseMove UpdateMousePos
            |> Collage.Events.onMouseUp DropSelection
    stackText = if model.faType == "PDA" then
      case model.currentFA of
        (FA _ _ _ cssFull _ _) -> case List.head cssFull of
          Nothing -> fromString "A stack:" |> size normal |> Collage.rendered
            |> shift (0, -265)
          Just (_, stack) -> fromString ("A stack: " ++ (case stack of
              Nothing -> Debug.todo "Not pda"
              Just (Left s) -> stackToString s
              Just (Right _) -> Debug.todo "Not pda")) |> 
            size normal |> Collage.rendered
            |> shift (0, -265)
      else fromString "" |> size normal |> Collage.rendered
    queueText = if model.faType == "QA" then
      case model.currentFA of
        (FA _ _ _ cssFull _ _) -> case List.head cssFull of
          Nothing -> fromString "A queue:" |> size normal |> Collage.rendered
            |> shift (0, -265)
          Just (_, queue) -> fromString ("A queue: " ++ (case queue of
              Nothing -> Debug.todo "Not qa"
              Just (Left _) -> Debug.todo "Not QA"
              Just (Right q) -> queueToString q)) |> 
            size normal |> Collage.rendered
            |> shift (0, -265)
      else fromString "" |> size normal |> Collage.rendered
    full_collage = group [states, edges, stackText, queueText, rect]
    status = case model.accepted of
      Nothing -> ""
      Just acc -> if acc then ", accepted" else ", rejected"
    getBulkAccept =
      let
        accStr = List.foldr (\(w, acc) str -> (if acc then (w ++ ", ") else "") ++ str) "" model.bulkResults
        accStrFormat = String.left ((String.length accStr) - 2) accStr
      in
        "Accepted: " ++ accStrFormat
    getBulkReject =
      let
        rejStr = List.foldr (\(w, acc) str -> (if not acc then (w ++ ", ") else "") ++ str) "" model.bulkResults
        rejStrFormat = String.left ((String.length rejStr) - 2) rejStr
      in
        "Rejected: " ++ rejStrFormat
    resColor = case model.accepted of
      Nothing -> Attr.style "color" "black"
      Just acc -> if acc then Attr.style "color" "green" else Attr.style "color" "red"
    runButtonType = if model.running then
        Button.outlineDark
        else Button.primary
    dfaButtonType = if model.faType == "DFA" then
        Button.warning
        else Button.dark
    nfaButtonType = if model.faType == "NFA" then
        Button.warning
        else Button.dark
    pdaButtonType = if model.faType == "PDA" then
        Button.warning
        else Button.dark
    qaButtonType = if model.faType == "QA" then
        Button.warning
        else Button.dark
    modal_header = if model.modalContent == "instructions" then
        "Instructions"
        else if model.modalContent == "errorUnconvertible" then
          "Error: Could not convert"
        else if model.modalContent == "loadFailure" then
          "Error: Could not load file"
        else
          "Unknown modal type"
    modal_body = if model.modalContent == "instructions" then
        Html.div [Attr.class "instructions"] [Html.text ("Instructions: Double click to add a state. " ++
          "Shift and drag from one state to another to add an edge. Select, " ++
          "drag, and drop states with the mouse. Quick keys: I: Toggle initial " ++
          "state. F: Toggle accepting state. Delete: Remove selected element." ++ 
          " Enter: Rename selected state or change transition characters for " ++
          "selected edge. Leave the transition characters empty for an epsilon " ++
          "transition in an NFA. PDA transitions are a list of items the format " ++
          "[stack head or empty]/[transition characers]/[Push to stack] separated" ++
          " by commas, ignoring whitespace. QA transitions are similar")]
      else if model.modalContent == "errorUnconvertible" then
        Html.div [Attr.class "instructions"] [Html.text ("You are trying to load" ++
          " a new " ++ model.faType ++ " but the automata file you selected was not" ++
          " a " ++ model.faType)]
      else if model.modalContent == "loadFailure" then
        Html.div [Attr.class "instructions"] [Html.text ("Your load attempt failed " ++
          "because the file you selected was not a properly formatted JSON automata.")]
      else
        Html.div [] []
    alert_modal = Modal.config CloseModal
            |> Modal.small
            |> Modal.h5 [] [ Html.text modal_header ]
            |> Modal.body []
                [ modal_body
                ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ Html.Events.onClick CloseModal ]
                    ]
                    [ Html.text "Close" ]
                ]
            |> Modal.view model.modalVisibility
    selectDiv = case model.currentSelect of
      Nothing -> Html.div [] []
      Just eith -> case eith of
        Left s -> case s of
          (N name _ iden) ->
            let
              initText = case isStateInitial model.currentFA s of
                True -> "Yes"
                False -> "No"
              acceptText = case isStateFinal model.currentFA s of
                True -> "Yes"
                False -> "No"
            in
              Html.div []
              [
              Html.div [Attr.class "selectTitle"] [ Html.text ("State Selected: " ++ name)],
              Html.div [Attr.class "lrWrapper"] [
                Html.input [ Attr.class "lSelect", Attr.placeholder "Pick name",
                  Attr.value model.reString, onInput ChangeString,
                  Html.Events.onFocus OnFocusReString,
                  Html.Events.onBlur OffFocus ] [],
                Button.button [ Button.outlineDark, Button.attrs [ Attr.class "rSelect",
                  Html.Events.onClick ReStringSelect ]]
                  [Html.text "Rename"]
              ],
              Html.div [Attr.class "lrWrapper"] [
                Html.div [Attr.class "lSelect"] [ Html.text ("Initial state: " ++ initText) ],
                Button.button [ Button.outlineDark, Button.attrs [ Attr.class "rSelect",
                  Html.Events.onClick ToggleInitSelect ]]
                  [Html.text "Toggle initial"]
              ],
              Html.div [Attr.class "lrWrapper"] [
                Html.div [Attr.class "lSelect"] [ Html.text ("Final state: " ++ acceptText) ],
                Button.button [ Button.outlineDark, Button.attrs [Attr.class "rSelect",
                  Html.Events.onClick ToggleFinalSelect ]]
                  [Html.text "Toggle final"]
              ],
              Button.button [ Button.danger, Button.attrs [ Attr.class "removeSelect",
                Html.Events.onClick RemoveSelect]]
                [Html.text "Remove state"]
              ]
        Right e -> case e of
          (E (from, to) transChars) ->
            let
              ss = case model.currentFA of
                (FA ss_ _ _ _ _ _) -> ss_
              fromName = case unwrap (getStateFromID ss from) of
                (N fName _ _) -> fName
              toName = case unwrap (getStateFromID ss to) of
                (N tName _ _) -> tName
            in
            Html.div [] [
              Html.div [Attr.class "selectTitle"] [ Html.text ("Edge Selected: From " ++ fromName ++ " to " ++ toName)],
              Html.input [ Attr.class "lSelect", Attr.style "margin-bottom" "8px",
                Attr.style "margin-top" "8px",
                Attr.style "width" "280px",
                Attr.id "edgeTransInput",
                Attr.placeholder "Pick transition characters",
                Attr.value model.reString, onInput ChangeString,
                Html.Events.onFocus OnFocusReString, Html.Events.onBlur OffFocus] [],
              Button.button [ Button.outlineDark, Button.attrs [ Attr.class "lSelect",
                Attr.style "margin-bottom" "8px",
                Html.Events.onClick ReStringSelect ]]
                [Html.text "Change characters"],
              Html.div [Attr.class "lSelect", Attr.style "color" "red"] (if model.showError then
                  [Html.text model.showErrorMsg]
                  else []),
              Button.button [ Button.danger, Button.attrs [ Attr.class "removeSelect",
                Html.Events.onClick RemoveSelect]]
                [Html.text "Remove edge"]
            ]
  in
    Html.div
      [
        Attr.style "margin-left" "auto",
        Attr.style "margin-right" "auto",
        Attr.class "grid-container",
        Attr.style "background-color" "#7D8B92",
        Attr.style "width" "100%",
        Attr.style "min-height" "100%"
      ]
      [ css_load "FSA_style.css",
        css_load "bootstrap_css.css",
        alert_modal,
      Html.div [Attr.class "TopBar"] [
          Button.button [ Button.small, dfaButtonType, Button.attrs [ Html.Events.onClick SwitchDFA,
            Attr.style "width" "200px", Attr.style "margin" "8px"]]
            [ Html.text "DFA" ],
          Button.button [ Button.small, nfaButtonType, Button.attrs [ Html.Events.onClick SwitchNFA,
            Attr.style "width" "200px", Attr.style "margin" "8px"]]
            [ Html.text "NFA" ],
          Button.button [ Button.small, pdaButtonType, Button.attrs [ Html.Events.onClick SwitchPDA,
            Attr.style "width" "200px", Attr.style "margin" "8px"]]
            [ Html.text "PDA" ],
          Button.button [ Button.small, qaButtonType, Button.attrs [ Html.Events.onClick SwitchQA,
            Attr.style "width" "200px", Attr.style "margin" "8px"]]
            [ Html.text "QA" ],
          Html.img [ Attr.src "automaton_simulator.png", Attr.class "titleImage"] []
      ],
      Html.div [
        Attr.style "-webkit-user-select" "none",
        Attr.style "-moz-user-select" "none",
        Attr.style "-ms-user-select" "none",
        Attr.style "user-select" "none",
        Attr.class "DFA",
        Attr.style "background-color" "white"
        ] [(svg  full_collage)],
      Html.div [Attr.class "Selection"] [selectDiv],
      Html.div [Attr.class "Controls"]
        (if model.controlScreen == 0 then
          [Html.div [Attr.class "lrWrapper"] [
            Html.div [Attr.class "controlTitle" ] [ Html.text "Controls: "],
            Button.button [ Button.small, Button.dark, Button.attrs [ Attr.class "rSelect",
              Html.Events.onClick NextControl]]
              [Html.text "\u{2192}"]
          ],
          Html.div [Attr.class "centerWrapper"] [
            Html.input [Attr.placeholder "Enter a word",
              Attr.class "inputCenter", Attr.value model.backWord,
              onInput ChangeWord, Html.Events.onFocus OnFocus,
              Html.Events.onBlur OffFocus  ] []
          ],
          Html.div [Attr.class "controlCenter"] [ Html.text (if model.running then
              (if model.currentSpeed == 10 then "Paused: " else "Running: ") ++ model.currentWord
              else ("Next word: " ++ model.currentWord))],
          Html.div [Attr.class "controlCenter"] [ Html.text "Set speed: (faster to slower)" ],
          Html.div [Attr.class "speedControl"] [ SingleSlider.view model.runSpeedSlider ],
          center (Button.button [ runButtonType, Button.attrs [ Html.Events.onClick RunWord
            ]]
            [ Html.text (if model.running then "Cancel" else "Run word") ]),
          Html.div [Attr.class "controlCenter"] [ Html.text ("Last word: " ++ model.lastWord),
            Html.span [ resColor ] [Html.text status ] ]
          ]
        else if model.controlScreen == 1 then
          [Html.div [Attr.class "lrWrapper"] [
            Button.button [ Button.small, Button.dark, Button.attrs [ Attr.class "topLeft",
              Html.Events.onClick PrevControl]]
              [Html.text "\u{2190}"],
            Html.div [Attr.class "controlTitleCenter" ] [ Html.text "Controls: "],
            Button.button [ Button.small, Button.dark, Button.attrs [ Attr.class "rSelect",
              Html.Events.onClick NextControl]]
              [Html.text "\u{2192}"]
          ],
          Html.textarea [Attr.class "bulkArea",
            Attr.placeholder "Enter many word separated by commas. Ignores whitespace.",
            Attr.value model.bulkWords,
            onInput ChangeBulk,
            Html.Events.onFocus OnFocus,
            Html.Events.onBlur OffFocus  ] [],
          center (Button.button [ runButtonType, Button.attrs [ Html.Events.onClick RunBulk
            ]]
            [ Html.text "Run all words" ]
          ),
          Html.div [Attr.class "lrWrapper"] [
            Html.textarea [Attr.class "halfBulkArea",
            (Attr.disabled True),
            Attr.style "color" "green",
            Attr.value getBulkAccept  ] [],
            Html.textarea [Attr.class "halfBulkArea",
            (Attr.disabled True),
            Attr.style "color" "red", Attr.value getBulkReject  ] []
            ]
          ]
        else 
          [Html.div [Attr.class "lrWrapper"] [
            Button.button [ Button.small, Button.dark, Button.attrs [ Attr.class "topLeft",
              Html.Events.onClick PrevControl]]
              [Html.text "\u{2190}"],
            Html.div [Attr.class "controlTitleRight" ] [ Html.text "Controls: "]
          ],
          Html.div [Attr.class "controlCenter"] [ Html.text getAlphabet ],
          Html.div [Attr.class "controlCenter"] [ Html.text getAlphabetDS ],
          Html.div [Attr.class "centerWrapper"] [
            Html.input [Attr.placeholder "Choose file name",
              Attr.class "inputCenter", Attr.value model.saveFile,
              onInput ChangeSaveFile, Html.Events.onFocus OnFocus,
              Html.Events.onBlur OffFocus  ] []
          ],
          center (Button.button [ Button.secondary, Button.attrs [ 
            Html.Events.onClick Save]]
            [Html.text "Save automata"]),
          center (Button.button [ Button.secondary, Button.attrs [ 
            Html.Events.onClick JSONFARequested]]
            [Html.text "Load automata"]),
          center (Button.button [ Button.danger, Button.attrs [ 
            Html.Events.onClick ClearBoard]]
            [Html.text "Clear Automata"]),
          center (if model.faType == "DFA" then
              Button.button [ Button.secondary, Button.attrs [ 
              Html.Events.onClick MinimizeMN]]
              [Html.text "Minimize DFA (Myhill-Nerode)"]
              else Html.div [] [])]
          )
      ]
  -- Html.text ("Pi: " ++ Debug.toString (4 * ((toFloat model.hitCount) / (toFloat (model.missCount + model.hitCount)))))

-- Function to load css from a path
css_load : String -> Html msg
css_load path =
    Html.node "link" [ Attr.rel "stylesheet", Attr.href path ] []

-- Functions to encode a FA in json
encodeState : State -> Encode.Value
encodeState (N name iden pos) =
  Encode.object [
    ("name", Encode.string name),
    ("iden", Encode.int iden),
    ("posX", Encode.float pos.x),
    ("posY", Encode.float pos.y)
  ]

encodeEdge : Edge -> Encode.Value
encodeEdge (E (from, to) trans) =
  Encode.object [
    ("from", Encode.int from),
    ("to", Encode.int to),
    ("trans", Encode.string trans)
  ]

encodeFA : FiniteAutomata -> String -> Encode.Value
encodeFA (FA states edges initStates _ finStates nextID) faType =
  Encode.object [
    ("states", Encode.list encodeState states),
    ("edges", Encode.list encodeEdge edges),
    ("initStates", Encode.list encodeState initStates),
    ("finStates", Encode.list encodeState finStates),
    ("nextID", Encode.int nextID),
    ("faType", Encode.string faType)
  ]

-- Functions to decode a FA from JSON
type alias DecodeState =
  {
    name : String,
    iden : Int,
    posX : Float,
    posY : Float
  }

type alias DecodeEdge =
  {
    from : Int,
    to : Int,
    trans : String
  }

type alias DecodeFiniteAutomata =
  {
    states : List DecodeState,
    edges : List DecodeEdge,
    initStates : List DecodeState,
    finStates : List DecodeState,
    nextID : Int,
    faType : String
  }

decodeStateToState : DecodeState -> State
decodeStateToState ds =
  (N ds.name ds.iden { x = ds.posX, y = ds.posY})

decodeEdgeToEdge : DecodeEdge -> Edge
decodeEdgeToEdge de =
  (E (de.from, de.to) de.trans)

decodeFAToFA : DecodeFiniteAutomata -> FiniteAutomata
decodeFAToFA dfa =
  let
    states = List.map decodeStateToState dfa.states
    edges = List.map decodeEdgeToEdge dfa.edges
    initStates = List.map decodeStateToState dfa.initStates
    finStates = List.map decodeStateToState dfa.finStates
  in
    (FA states edges initStates [] finStates dfa.nextID)

decodeState : Decode.Decoder DecodeState
decodeState = 
  Decode.succeed DecodeState
    |> required "name" Decode.string
    |> required "iden" Decode.int
    |> required "posX" Decode.float
    |> required "posY" Decode.float

decodeEdge : Decode.Decoder DecodeEdge
decodeEdge =
  Decode.succeed DecodeEdge
    |> required "from" Decode.int
    |> required "to" Decode.int
    |> required "trans" Decode.string

decodeFA : Decode.Decoder DecodeFiniteAutomata
decodeFA =
  Decode.succeed DecodeFiniteAutomata
    |> required "states" (Decode.list decodeState)
    |> required "edges" (Decode.list decodeEdge)
    |> required "initStates" (Decode.list decodeState)
    |> required "finStates" (Decode.list decodeState)
    |> required "nextID" Decode.int
    |> required "faType" Decode.string

downloadFA : String -> FiniteAutomata -> String -> Cmd Msg
downloadFA filename fa faType =
  Download.string filename "application/json" (Encode.encode 0 (encodeFA fa faType))
  
requestFA : Cmd Msg
requestFA =
  Select.file ["application/json"] JSONFALoaded
