# Automaton-Simulator

This is a simulator for several abstract machines in automata theory and formal languages written in Elm for CMSC 22300: Introduction to Functional Programming at UChicago. There were two main components to this: an Elm file with an automaton data type and functions to construct and run an automaton (StateMachines.elm) and an Elm file to display a visual interface which the user could use to easily manipulate the simulator (FSA.elm).

## How to run:

You can see a (somewhat-out-of-date) version running at: https://www.classes.cs.uchicago.edu/archive/2020/spring/22300-1/showcase/amrivkin/index.html

Alternatively, you can run the code locally with Elm. See: https://www.classes.cs.uchicago.edu/archive/2015/winter/22300-1/lectures/IntroML.html and https://www.classes.cs.uchicago.edu/archive/2015/winter/22300-1/lectures/IntroFRP.html for how to do this.

## Abstract machines:

I include four types of abstract machines, deterministic finite automata (DFA), non-deterministic finite automata (NFA), pushdown automata (PDA), and queue automata (QA).

A DFA is a set of states, an initial state, a set of accepting states, and transitions between states such that given an input character, a state can transition to at most one state (Normally, a DFA requires an alphabet too, and that there is a transition defined for every character in that alphabet at every state. For the simulator, the alphabet is the set of all unicode characters which the browser can recognize, and there is an implicit, non-accepting state which is the output of any transition for a state/character pair which the user hasn't been defined yet).

A NFA is similar to a DFA except that multiple transitions can be defined for the same state/character pair, in which case the machine takes all possible transitions at once. Also, an NFA allows epsilon transitions which are taken without reading a character.

A PDA is an NFA which maintains a stack for each current non-deterministic state while running and pushes or pops from the state while transitioning. My simulator does not allow epsilon transitions for PDAs to avoid the risk of non-terminating behavior (Apparently, PDAs without
epsilon transitions are equivalent to PDAs with epsilons, according one Stack Exchange post: https://cs.stackexchange.com/questions/55875/removing-epsilon-transitions-in-a-npda, but I'd have to look into that more before being convinced).

QAs are PDAs with a queue instead of a stack. Queue automata with epsilon transitions are equivalent to Turing Machines, but my simulator does not allow epsilons for QAs.

## Interface:

The interface interface has three primary components: a collage, a control box, and a selection box where the user can edit the currently selected collage component.

The user can add states by double clicking on the collage and add edges by dragging between two states while holding down shift. Also, they can move states by clicking and dragging while not holding shift. A state or edge can be selected by clicking on it. This will show the component's info in the selection box. The selection box allows states to be renamed (although only the first four characters of the name will show up in the collage due to size constraints), made initial, made accepting, or deleted. For edges, it allows the user to change the transition characters of an edge.

The user can use the keyboard to quickly manipulate the selected component too ("i" to toggle initial state, "f" to toggle accepting
state, "delete" to remove a state).

The control box allows the user to run individual words with adjustable speeds, run many words at once instantly, see the input alphabet and stack/queue
alphabet actively used in the current machine, and reset the collage.
