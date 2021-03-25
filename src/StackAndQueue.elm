module StackAndQueue exposing
  (Queue, Stack, emptyQueue, isEmptyQueue, emptyStack, isEmptyStack,
  pushQueue, pushStack, popQueue, popStack, peekQueue, peekStack,
  pushStackList, pushQueueList, queueToString, stackToString)

-- Implement stack and queue data structures for the PDA and QA

type Queue a =
  Q { front : List a, back : List a }

type alias Stack a = List a

emptyQueue : Queue a
emptyQueue = Q {front = [], back = []}

isEmptyQueue : Queue a -> Bool
isEmptyQueue q =
  q == emptyQueue

checkFront : List a -> List a -> Queue a
checkFront f b =
  case f of
    [] -> Q {front = List.reverse b, back = []}
    _  -> Q {front = f, back = b}

pushQueue : a -> Queue a -> Queue a
pushQueue x (Q {front, back}) =
  checkFront front (x::back)
  
peekQueue : Queue a -> Maybe a
peekQueue (Q {front, back}) =
  List.head front

popQueue : Queue a -> Maybe (Queue a)
popQueue (Q {front, back}) =
  case front of
    []    -> Nothing
    _::f_ -> Just (checkFront f_ back)

emptyStack : Stack a
emptyStack = []

isEmptyStack : Stack a -> Bool
isEmptyStack s =
  s == emptyStack
  
pushStack : a -> Stack a -> Stack a 
pushStack x s = x :: s

peekStack : Stack a -> Maybe a
peekStack s = case s of
  [] -> Nothing
  x :: _ -> Just x

popStack : Stack a -> Maybe (Stack a)
popStack s = case s of
  [] -> Nothing
  _ :: rest -> Just rest

pushStackList : List a -> Stack a -> Stack a
pushStackList ls stack = case ls of
  [] -> stack
  a::rest -> pushStackList rest (pushStack a stack)

pushQueueList : List a -> Queue a -> Queue a
pushQueueList ls queue = case ls of
  [] -> queue
  a::rest -> pushQueueList rest (pushQueue a queue)

stackToString : Stack Char -> String
stackToString stack =
  let
    helper s counter = if counter <= 0 then
      ""
      else case peekStack s of
        Nothing -> ""
        Just c -> (String.fromChar c) ++ " " ++ helper (unwrap (popStack s)) (counter - 1)
  in
    helper stack 20

queueToString : Queue Char -> String
queueToString queue =
  let
    helper q counter = if counter <= 0 then
      ""
      else case peekQueue q of
        Nothing -> ""
        Just c -> (String.fromChar c) ++ " " ++ helper (unwrap (popQueue q)) (counter - 1)
  in
    helper queue 20

unwrap : Maybe a -> a
unwrap ma = case ma of
  Nothing -> Debug.todo "Shouldn't have unwrapped"
  Just x -> x

