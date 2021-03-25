module StateMachines exposing (..)

import StackAndQueue exposing (..)

-- Type representing a finite automaton with states, edges, current states, current transitions, current stacks or queues, and an identifier
type FiniteAutomata = FA (List State) (List Edge) (List State) (List (State, Maybe (Either (Stack Char) (Queue Char)))) (List State) Int

-- Type for a state/node with a title, identifier, and location
type State = N String Int Point

-- Type for an edge between two state identifiers and with transition information
type Edge = E (Int, Int) String

type alias Point = { x:Float, y:Float }

-- Types of finite automata supported by the simulator
type MachineType = DFA
  | NFA
  | PDA
  | QA

type Either a b
    = Left a
    | Right b

-- Init a FA
initFA : FiniteAutomata
initFA =
  FA [] [] [] [] [] 0

-- Extracts the current states of an automata
extractCurFinite : List (State, Maybe (Either (Stack Char) (Queue Char))) -> List State
extractCurFinite csFull =
  case csFull of
    [] -> []
    (s, _) :: rest -> s :: (extractCurFinite rest)

-- Helper function for DFAs and NFAs with no stacks or queues
wrapCurFinite : List State -> List (State, Maybe (Either (Stack Char) (Queue Char)))
wrapCurFinite cs =
  List.map (\x -> (x, Nothing)) cs

-- Retrieve a state from its identifiers
getStateFromID : List State -> Int -> Maybe State
getStateFromID states iden =
  case states of
    [] -> Nothing
    s::restStates -> case s of
      (N _ sIden _) -> if sIden == iden then Just s else getStateFromID restStates iden

-- Checks if in an initial state
isStateInitial : FiniteAutomata -> State -> Bool
isStateInitial (FA _ _ initStates _ _ _) (N _ iden _) =
  case getStateFromID initStates iden of
    Nothing -> False
    Just _ -> True

-- Checks if in a final state
isStateFinal : FiniteAutomata -> State -> Bool
isStateFinal (FA _ _ _ _ finStates _) (N _ iden _) =
  case getStateFromID finStates iden of
    Nothing -> False
    Just _ -> True

-- Updates a FA's states with new information
changeStateData : FiniteAutomata -> State -> String -> Point -> FiniteAutomata
changeStateData (FA states edges initStates curStates finStates nextID) s newName newP =
  if List.member s states then
      let
        newS = case s of
          (N _ iden _) -> N newName iden newP
        newStates = newS :: (List.filter (\x -> x /= s) states)
        newFin = if List.member s finStates then
          newS :: (List.filter (\x -> x /= s) finStates)
          else finStates
        newInit = if List.member s initStates then
          newS :: (List.filter (\x -> x /= s) initStates)
          else initStates
        newCur = List.map (\(x, ds) -> if x == s then (s, ds) else (x, ds)) curStates
      in
        FA newStates edges newInit newCur newFin nextID
    else FA states edges initStates curStates finStates nextID

changeStateDataIndividual : State -> String -> Point -> State
changeStateDataIndividual (N _ iden _) newName newP =
  N newName iden newP

-- Updates a FA's edges with new information
changeEdgeData : FiniteAutomata -> Edge -> String -> FiniteAutomata
changeEdgeData (FA states edges initStates curStates finStates nextID) e newTrans =
  if List.member e edges then
      let
        newE = case e of
          (E (from, to) _) -> E (from, to) newTrans
        newEdges = newE :: (List.filter (\x -> x /= e) edges)
      in
        FA states newEdges initStates curStates finStates nextID
    else FA states edges initStates curStates finStates nextID
    
changeEdgeDataIndividual : Edge -> String -> Edge
changeEdgeDataIndividual (E (from, to) _) newTrans =
  E (from, to) newTrans

-- Retrieves a state from a point
getStateFromPos : List State -> Point -> Maybe State
getStateFromPos states point =
  case states of
    [] -> Nothing
    s::restStates -> case s of
      (N _ _ sPoint) -> if point == sPoint then Just s else getStateFromPos restStates point

-- Distance between two points
pointDistance : Point -> Point -> Float
pointDistance p1 p2 =
  sqrt ((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))

-- Gets state very close to a point
getStateFromApproxPos : List State -> Point -> Maybe State
getStateFromApproxPos states point =
  case states of
    [] -> Nothing
    s::restStates -> case s of
      (N _ _ sPoint) -> if pointDistance point sPoint < 0.05 then
        Just s
        else getStateFromApproxPos restStates point

-- Many straightforward helpers

getEdgeFromFT : List Edge -> Int -> Int -> Maybe Edge
getEdgeFromFT edges from to =
  case edges of
    [] -> Nothing
    e::restEdges -> case e of
      (E (eFrom, eTo) _) -> if eFrom == from && eTo == to then Just e else getEdgeFromFT restEdges from to

clearCurrent : FiniteAutomata -> FiniteAutomata
clearCurrent (FA states edges initStates _ finStates nextID) =
  (FA states edges initStates [] finStates nextID)

addFinal : State -> FiniteAutomata -> FiniteAutomata
addFinal s (FA states edges initStates curStates finStates nextID) =
  case List.member s finStates of
    True -> FA states edges initStates curStates finStates nextID
    False -> FA states edges initStates curStates (s :: finStates) nextID

removeFinal : State -> FiniteAutomata -> FiniteAutomata
removeFinal s (FA states edges initStates curStates finStates nextID) =
  let
    new_finStates = List.filter (\x -> s /= x) finStates
  in
    FA states edges initStates curStates new_finStates nextID

setInitialStateDFA : State -> FiniteAutomata -> FiniteAutomata
setInitialStateDFA s (FA states edges initStates curStates finStates nextID) =
  if List.member s states then FA states edges [s] curStates finStates nextID
    else FA states edges initStates curStates finStates nextID

removeInitialStateDFA : FiniteAutomata -> FiniteAutomata
removeInitialStateDFA (FA states edges initStates curStates finStates nextID) =
  FA states edges [] curStates finStates nextID

addInitialStateNFA : State -> FiniteAutomata -> FiniteAutomata
addInitialStateNFA s (FA states edges initStates curStates finStates nextID) =
  if List.member s states then
    if List.member s initStates then
      FA states edges initStates curStates finStates nextID
    else
      FA states edges (s :: initStates) curStates finStates nextID
  else FA states edges initStates curStates finStates nextID

removeInitialStateNFA : State -> FiniteAutomata -> FiniteAutomata
removeInitialStateNFA s (FA states edges initStates curStates finStates nextID) =
  let
    newInit = List.filter (\x -> x /= s) initStates 
  in
    (FA states edges newInit curStates finStates nextID)

removeState : State -> FiniteAutomata -> FiniteAutomata
removeState s (FA states edges initStates curStates finStates nextID) =
  if List.member s states then
      let
        newStates = List.filter (\x -> x /= s) states
        newFin = List.filter (\x -> x /= s) finStates
        sIden = case s of
          N _ iden _ -> iden
        newEdges = List.filter (\(E (from, to) _) -> from /= sIden && to /= sIden) edges
        newInit = List.filter (\x -> x /= s) initStates
        newCur = List.filter (\(x, _) -> x /= s) curStates
      in
        FA newStates newEdges newInit newCur newFin nextID
    else FA states edges initStates curStates finStates nextID

addState : String -> Point -> FiniteAutomata -> FiniteAutomata
addState name pos (FA states edges initStates curStates finStates nextID) =
  let
    s = N name nextID pos
  in
    if states == [] then FA [s] edges [s] curStates finStates (nextID + 1)
      else FA (s :: states) edges initStates curStates finStates (nextID + 1)

removeEdge : Edge -> FiniteAutomata -> FiniteAutomata
removeEdge e (FA states edges initStates curStates finStates nextID) =
  if List.member e edges then
      let
        newEdges = List.filter (\x -> x /= e) edges
      in
        FA states newEdges initStates curStates finStates nextID
    else FA states edges initStates curStates finStates nextID

addEdge : Int -> Int -> String -> FiniteAutomata -> FiniteAutomata
addEdge from to transChars (FA states edges initStates curStates finStates nextID) =
  case getEdgeFromFT edges from to of
    Nothing -> FA states ((E (from, to) transChars) :: edges) initStates curStates finStates nextID
    Just _ -> (FA states edges initStates curStates finStates nextID)


isCharInEdge : Char -> Edge -> Bool
isCharInEdge c (E _ transChars) =
  List.member c (String.toList transChars)

-- Used to get epsilon closure of a state
getEpsilonClosure : State -> FiniteAutomata -> List State -> List State
getEpsilonClosure s fa already =
  case fa of
    (FA states edges _ _ _ _) ->
      let
        edgesOfState = List.filter (isEdgeFromState s) edges
        epsilonEdges = List.filter (\(E _ transChars) -> String.length transChars == 0) edgesOfState
        othersClosureWithOld = List.map (\(E (_, to) _) -> unwrap (getStateFromID states to)) epsilonEdges
        othersClosure = List.filter (\x -> not (List.member x already)) (s :: othersClosureWithOld)
        singleClosure = stripDuplicates (othersClosure)
        newAlready = already ++ singleClosure
      in
        case singleClosure of
          [] -> []
          _ -> singleClosure ++ (List.concat (List.map (\x -> getEpsilonClosure x fa newAlready) singleClosure))

isEdgeFromState : State -> Edge -> Bool
isEdgeFromState (N _ iden _) (E (from, _) _) =
  iden == from


getEdgeMatches : Maybe Char -> State -> List Edge -> List Edge
getEdgeMatches wrapped_c s edges =
  case wrapped_c of
    Nothing -> []
    Just c ->
      let
        edgesOfState = List.filter (isEdgeFromState s) edges
      in
        List.filter (isCharInEdge c) edgesOfState
        
parseTransChars : String -> List (Maybe Char, String, String)
parseTransChars trans =
  let
    transs = splitBulk trans
  in
    List.map (\t -> case (String.split "/" t) of
      [dsTake, transChars, dsGive] -> case (String.toList dsTake) of
        [] -> (Nothing, transChars, dsGive)
        [dsT] -> (Just dsT, transChars, dsGive)
        _ -> Debug.todo "improperly formatted trans"
      _ -> Debug.todo "improperly formatted trans") transs

possibleParseTransChars : String -> Bool
possibleParseTransChars trans =
  let
    transs = splitBulk trans
  in
    List.foldl (&&) True (List.map (\t -> case (String.split "/" t) of
      [dsTake, transChars, dsGive] -> case (String.toList dsTake) of
        [] -> True
        [dsT] -> True
        _ -> False
      _ -> False) transs)

getValidPDA : Char -> Stack Char -> Edge -> List (Int, Stack Char)
getValidPDA c stack (E (_, to) transChars) =
  let
    transs = parseTransChars transChars
  in
    stripDuplicates (List.concat (List.map (\(stc, trans, sgs) ->
      case stc of
        Nothing -> if List.member c (String.toList trans) then
          [(to, pushStackList (String.toList sgs) stack)]
          else []
        Just stc_ -> if (peekStack stack == Just stc_) && List.member c (String.toList trans) then
          [(to, pushStackList (String.toList sgs) (unwrap (popStack stack)))]
          else []) transs))

getValidQA : Char -> Queue Char -> Edge -> List (Int, Queue Char)
getValidQA c queue (E (_, to) transChars) =
  let
    transs = parseTransChars transChars
  in
    stripDuplicates (List.concat (List.map (\(qtc, trans, qgs) ->
      case qtc of
        Nothing -> if List.member c (String.toList trans) then
          [(to, pushQueueList (String.toList qgs) queue)]
          else []
        Just qtc_ -> if (peekQueue queue == Just qtc_) && List.member c (String.toList trans) then
          [(to, pushQueueList (String.toList qgs) (unwrap (popQueue queue)))]
          else []) transs))

getNewCurPDA : Maybe Char -> (State, Stack Char) -> List Edge -> List (Int, Stack Char)
getNewCurPDA wrapped_c (s, stack) edges =
  case wrapped_c of
    Nothing -> []
    Just c ->
      let
        edgesOfState = List.filter (isEdgeFromState s) edges
      in
        stripDuplicates (List.concat (List.map (getValidPDA c stack) edgesOfState))

getNewCurQA: Maybe Char -> (State, Queue Char) -> List Edge -> List (Int, Queue Char)
getNewCurQA wrapped_c (s, queue) edges =
  case wrapped_c of
    Nothing -> []
    Just c ->
      let
        edgesOfState = List.filter (isEdgeFromState s) edges
      in
        stripDuplicates (List.concat (List.map (getValidQA c queue) edgesOfState))
    
startRunDFA : FiniteAutomata -> FiniteAutomata
startRunDFA fa =
  case fa of
    (FA states edges initStates curStates finStates nextID) ->
      FA states edges initStates (wrapCurFinite initStates) finStates nextID
      
startRunNFA : FiniteAutomata -> FiniteAutomata
startRunNFA fa =
  case fa of
    (FA states edges initStates curStates finStates nextID) ->
      FA states edges initStates (wrapCurFinite (getEpsilonClosureAll initStates fa)) finStates nextID

startRunPDA : FiniteAutomata -> FiniteAutomata
startRunPDA fa =
  case fa of
    (FA states edges initStates curStates finStates nextID) ->
      FA states edges initStates (List.map (\x -> (x, Just (Left emptyStack))) initStates) finStates nextID

startRunQA : FiniteAutomata -> FiniteAutomata
startRunQA fa =
  case fa of
    (FA states edges initStates curStates finStates nextID) ->
      FA states edges initStates (List.map (\x -> (x, Just (Right emptyQueue))) initStates) finStates nextID

getEpsilonClosureAll : List State -> FiniteAutomata -> List State
getEpsilonClosureAll ss fa =
  stripDuplicates (List.concat (List.foldl (\s acc -> (getEpsilonClosure s fa []) :: acc) [] ss))
  
runBulkDFA : List String -> FiniteAutomata -> List (String, Bool)
runBulkDFA words fa =
  List.map (\w -> (w, runDFA w fa)) words

runBulkNFA : List String -> FiniteAutomata -> List (String, Bool)
runBulkNFA words fa =
  List.map (\w -> (w, runNFA w fa)) words

runBulkPDA : List String -> FiniteAutomata -> List (String, Bool)
runBulkPDA words fa =
  List.map (\w -> (w, runPDA w fa)) words

runBulkQA : List String -> FiniteAutomata -> List (String, Bool)
runBulkQA words fa =
  List.map (\w -> (w, runQA w fa)) words

splitBulk : String -> List String
splitBulk bulkWords =
  let
    noWhitespace = String.filter (\c -> not (List.member c [' ', '\t', '\n'])) bulkWords
  in
    String.split "," noWhitespace

runFA : String -> MachineType -> FiniteAutomata -> Bool
runFA word faType fa =
  let
    start = case faType of
      DFA -> startRunDFA fa
      NFA -> startRunNFA fa
      PDA -> startRunPDA fa
      QA -> startRunQA fa
    updateFunc = case faType of
      DFA -> updateDFA
      NFA -> updateNFA
      PDA -> updatePDA
      QA -> updateQA
    updateAll (wrapped_w, wrapped_step) =
      case wrapped_w of
        Nothing -> ("", wrapped_step)
        Just w -> case wrapped_step of
          Nothing -> (w, Nothing)
          Just step -> case String.toList w of
            [] -> (w, wrapped_step)
            _ -> updateAll (updateFunc w step)
  in
    accept (Tuple.second (updateAll (Just word, Just start)))
    
runDFA : String -> FiniteAutomata -> Bool
runDFA word fa =
  runFA word DFA fa

runNFA : String -> FiniteAutomata -> Bool
runNFA word fa =
  runFA word NFA fa

runPDA : String -> FiniteAutomata -> Bool
runPDA word fa =
  runFA word PDA fa

runQA : String -> FiniteAutomata -> Bool
runQA word fa =
  runFA word QA fa

accept : Maybe FiniteAutomata -> Bool
accept wrapped_fa =
  case wrapped_fa of
    Nothing -> False
    Just (FA states edges initStates curStates finStates nextID) ->
      List.foldr (||) False (List.map (\x -> List.member x finStates) (extractCurFinite curStates))

edgeToString : Edge -> String
edgeToString (E (from, to) transChars) =
  transChars ++ ", from " ++ String.fromInt from ++ ", to " ++  String.fromInt to

stateToString : State -> String
stateToString (N name iden _) =
  name ++ ", identifier " ++ String.fromInt iden

-- Helper for running a FA
getNewCurs : MachineType -> Char ->
  (List (State, Maybe (Either (Stack Char) (Queue Char)))) -> FiniteAutomata
  -> (List (State, Maybe (Either (Stack Char) (Queue Char))))
getNewCurs faType c cssFull fa =
  case fa of
    (FA states edges _ _ _ _) ->
      case faType of
        DFA ->
          let
            cs = unwrap (List.head (extractCurFinite cssFull))
            edgeMatches = getEdgeMatches (Just c) cs edges
            len = List.length edgeMatches
          in
            if len == 0 then []
              else if len == 1 then case edgeMatches of
                [(E (_, to) _)] -> wrapCurFinite [unwrap (getStateFromID states to)]
                _ -> Debug.todo "Will not occur because length is one"
              else []
        NFA ->
          let
            css = extractCurFinite cssFull
            getEdgeTos es = List.map (\(E (_, to) _) -> to) es
            newIDs = List.concat (List.foldl (\cs acc -> (getEdgeTos (getEdgeMatches (Just c) cs edges)) :: acc) [] css)
            intToStates idens = case idens of
              [] -> []
              iden :: restIdens -> if List.member iden restIdens then intToStates restIdens
                else unwrap (getStateFromID states iden) :: (intToStates restIdens)
          in
            wrapCurFinite (getEpsilonClosureAll (intToStates newIDs) fa)
        PDA ->
          let
            newIDStacks = List.concat (List.foldl (\cs acc -> (getNewCurPDA (Just c) cs edges) :: acc) [] (toAllStack cssFull))
            intToStates idensStacks = case idensStacks of
              [] -> []
              (iden, stack) :: restIdens -> if List.member (iden, stack) restIdens then intToStates restIdens
                else (unwrap (getStateFromID states iden), stack ) :: (intToStates restIdens)
          in
            fromStackToDS (intToStates newIDStacks)
        QA ->
          let
            newIDQueues = List.concat (List.foldl (\cs acc -> (getNewCurQA (Just c) cs edges) :: acc) [] (toAllQueue cssFull))
            intToStates idensQueues = case idensQueues of
              [] -> []
              (iden, queue) :: restIdens -> if List.member (iden, queue) restIdens then intToStates restIdens
                else (unwrap (getStateFromID states iden), queue ) :: (intToStates restIdens)
          in
            fromQueueToDS (intToStates newIDQueues)

-- Function for each update of a FA as a character is read
updateFA : String -> MachineType -> FiniteAutomata -> (Maybe String, Maybe FiniteAutomata)
updateFA word faType (FA states edges initStates curStates finStates nextID) =
  let
    makeCur newCur = Just (FA states edges initStates newCur finStates nextID)
  in
    case String.toList word of
      [] -> (Nothing, makeCur curStates)
      c :: restWord ->
        let
          wrapped_css = case curStates of
            [] -> Nothing
            css_ -> Just css_
        in
          case wrapped_css of
            Nothing -> (Just (String.fromList restWord), makeCur curStates)
            Just css ->
              let
                newCurs = getNewCurs faType c css (unwrap (makeCur curStates))
              in
                case newCurs of
                  [] -> (Nothing, makeCur newCurs)
                  _ -> (Just (String.fromList restWord), makeCur newCurs)


updateDFA : String -> FiniteAutomata -> (Maybe String, Maybe FiniteAutomata)
updateDFA word fa =
  updateFA word DFA fa

updateNFA : String -> FiniteAutomata -> (Maybe String, Maybe FiniteAutomata)
updateNFA word fa =
  updateFA word NFA fa
                  
toAllStack : List (State, Maybe (Either (Stack Char) (Queue Char))) -> List (State, Stack Char)
toAllStack ls =
  case ls of
    [] -> []
    (s, ds)::rest -> case ds of
      Nothing -> Debug.todo "Not called on PDA"
      Just (Left stack) -> (s, stack) :: (toAllStack rest)
      Just (Right _) -> Debug.todo "Not called on PDA"

fromStackToDS : List (State, Stack Char) -> List (State, Maybe (Either (Stack Char) (Queue Char)))
fromStackToDS ls =
  List.map (\(s, stack) -> (s, Just (Left stack))) ls

updatePDA : String -> FiniteAutomata -> (Maybe String, Maybe FiniteAutomata)
updatePDA word fa =
  updateFA word PDA fa

toAllQueue : List (State, Maybe (Either (Stack Char) (Queue Char))) -> List (State, Queue Char)
toAllQueue ls =
  case ls of
    [] -> []
    (s, ds)::rest -> case ds of
      Nothing -> Debug.todo "Not called on QA"
      Just (Left _) -> Debug.todo "Not called on QA"
      Just (Right queue) -> (s, queue) :: (toAllQueue rest)

fromQueueToDS : List (State, Queue Char) -> List (State, Maybe (Either (Stack Char) (Queue Char)))
fromQueueToDS ls =
  List.map (\(s, queue) -> (s, Just (Right queue))) ls

updateQA : String -> FiniteAutomata -> (Maybe String, Maybe FiniteAutomata)
updateQA word fa =
  updateFA word QA fa

unwrap : Maybe a -> a
unwrap ma = case ma of
  Nothing -> Debug.todo "Shouldn't have unwrapped"
  Just x -> x

stripDuplicates : List a -> List a
stripDuplicates input =
  List.foldr (\x prev -> x :: (List.filter (\y -> y /= x) prev)) input input
  
sumTrans : List Edge -> List Char
sumTrans es =
  stripDuplicates (List.concat (List.map (\(E _ trans) -> String.toList trans) es))
  
sumTransInputDS : List Edge -> List Char
sumTransInputDS es =
  stripDuplicates (List.concat (List.map (\(E _ transFull) ->
    List.concat (List.map (\transList -> case transList of
    (_, trans, _) -> String.toList trans) (parseTransChars transFull))) es))
    
sumTransDS : List Edge -> List Char
sumTransDS es =
  stripDuplicates (List.concat (List.map (\(E _ transFull) ->
    List.concat (List.map (\transList -> case transList of
    (Nothing, _, dsg) -> String.toList dsg
    (Just dst, _, dsg) -> dst :: String.toList dsg) (parseTransChars transFull))) es))

-- WIP: Functions to find a minimal DFA equivalent to the current DFA through Hopcroft's algorithm
minimizeMN : FiniteAutomata -> FiniteAutomata
minimizeMN fa =
  case fa of
    (FA states edges _ _ finStates _) ->
      let
        len = List.length states
        prefixes = List.map (\n -> List.drop (n + 1) states) (List.range 0 len)
        spPairs = List.map2 Tuple.pair states prefixes
        statePairs = List.concat (List.map (\(s, pre) ->
          if List.member s finStates then
            List.map (\x ->
              if List.member x finStates then
                ((Just s, Just x), False)
              else ((Just s, Just x), True)) pre
            else List.map (\x ->
              if List.member x finStates then
                ((Just s, Just x), True)
              else ((Just s, Just x), False)) pre) spPairs)
      in
        minimizeMNHelper statePairs fa (sumTrans edges)

minimizeMNHelper : List ((Maybe State, Maybe State), Bool) -> FiniteAutomata -> List Char -> FiniteAutomata
minimizeMNHelper statePairs fa alphabet =
  case fa of
    (FA states edges _ _ _ _) ->
      let
        sOfChar c s = case getEdgeMatches (Just c) (unwrap s) edges of
          [] -> Nothing
          [e] -> case e of
            (E (_, to) _) -> getStateFromID states to
          _ -> Nothing
        newStatePairs = List.map (\((s1, s2), b) -> if b then
            ((s1, s2), b)
          else
            ((s1, s2), (List.foldr (\c prev -> prev ||
              List.member ((sOfChar c s1, sOfChar c s2), True) statePairs) False alphabet))) statePairs
        oldTrueCount = List.foldr (\(_, b) acc -> if b then acc + 1 else acc) 0 statePairs
        newTrueCount = List.foldr (\(_, b) acc -> if b then acc + 1 else acc) 0 newStatePairs
      in
        if oldTrueCount == newTrueCount then
          condenseMN newStatePairs fa
          else minimizeMNHelper newStatePairs fa alphabet

condenseMN : List ((Maybe State, Maybe State), Bool) -> FiniteAutomata -> FiniteAutomata
condenseMN statePairs fa =
  case fa of
    (FA states edges initStates curStates finStates nextID) ->
      let
        getUnmarked sps = case sps of
          [] -> Nothing
          ((s1, s2), False) :: _ -> Just (unwrap s1, unwrap s2)
          (_, True) :: rest_sps -> getUnmarked rest_sps
      in
        case getUnmarked statePairs of
          Nothing -> fa
          Just (s1, s2) ->
            let
              wrap_s1 = Just s1
              wrap_s2 = Just s2
              newStatePairs = List.map (\((sp_s1, sp_s2), b) ->
                if sp_s1 == wrap_s1 && sp_s2 == wrap_s2 then ((wrap_s1, wrap_s2), True)
                else if sp_s1 == wrap_s2 then ((wrap_s1, sp_s2), b)
                else if sp_s2 == wrap_s2 then ((sp_s1, wrap_s1), b)
                else ((sp_s1, sp_s2), b)
                 ) statePairs
            in
              condenseMN newStatePairs (mergeStates s2 s1 fa)

mergeStates : State -> State -> FiniteAutomata -> FiniteAutomata
mergeStates sOld sNew (FA states edges initStates curStates finStates nextID) =
  if List.member sOld states then
    let
      newStates = sNew :: (List.filter (\x -> x /= sOld && x /= sNew) states)
      newFin = if List.member sNew finStates || List.member sOld finStates then
        sNew :: (List.filter (\x -> x /= sOld && x /= sNew) finStates)
        else finStates
      newInit = if List.member sNew initStates || List.member sOld initStates then
        sNew :: (List.filter (\x -> x /= sOld && x /= sNew) initStates)
        else initStates
      newCur = List.filter (\(x, _) -> x /= sOld && x /= sNew) curStates
      sIdenOld = case sOld of
        N _ iden _ -> iden
      sIdenNew = case sNew of
        N _ iden _ -> iden
      newEdgesDup = List.map (\(E (from, to) trans) ->
        if from == sIdenOld && to == sIdenOld then E (sIdenNew, sIdenNew) trans
          else if from == sIdenOld then E (sIdenNew, to) trans
          else if to == sIdenOld then E (from, sIdenNew) trans
          else E (from, to) trans) edges
      newEdges = mergeDuplicateEdges newEdgesDup
    in
      FA newStates newEdges newInit newCur newFin nextID
    else FA states edges initStates curStates finStates nextID

mergeDuplicateEdges : List Edge -> List Edge
mergeDuplicateEdges input =
  List.foldr (\(E (from, to) _) prev ->
    let
      addTrans = List.foldr (\(E (fromE, toE) transE) prevTrans ->
        if (from, to) == (fromE, toE) then
          transE ++ prevTrans
          else prevTrans ) "" prev
    in
      (E (from, to) (String.fromList (stripDuplicates (String.toList addTrans)))) ::
        (List.filter (\(E (fromE, toE) _) -> (from, to) /= (fromE, toE)) prev)) input input
