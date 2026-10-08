open! Core

type player_kind =
  | North
  | East
  | South
  | West

type suit =
  | Clubs
  | Diamonds
  | Spades
  | Hearts

type card =
  { suit : suit
  ; rank : int
  }

type decision =
  | In_progress of { whose_turn : player_kind }
  | Winner of player_kind
  | Stalemate

type game_state =
  { trick : (card * player_kind) list
  ; hearts_broken : bool
  ; trick_number : int
  ; target_score : int
  ; decision : decision
  }

type move = card

val initial_state : game_state
val move_two_of_clubs : move
val state_after_first_move : game_state
val before_terminal_state : game_state
val move_to_terminal_state : move
val terminal_state : game_state