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

let initial_state : game_state =
  { trick = []
  ; hearts_broken = false
  ; trick_number = 1
  ; target_score = 100
  ; decision = In_progress { whose_turn = North }
  }
;;

let move_two_of_clubs : move = { suit = Clubs; rank = 2 }

let state_after_first_move : game_state =
  { trick = [ move_two_of_clubs, North ]
  ; hearts_broken = false
  ; trick_number = 1
  ; target_score = 100
  ; decision = In_progress { whose_turn = East }
  }
;;

let before_terminal_state : game_state =
  { trick =
      [ { suit = Clubs; rank = 14 }, North  (* Ace of Clubs *)
      ; { suit = Clubs; rank = 5 }, East
      ; { suit = Clubs; rank = 9 }, South
      ]
  ; hearts_broken = true
  ; trick_number = 13
  ; target_score = 100
  ; decision = In_progress { whose_turn = West }
  }
;;

let move_to_terminal_state : move = { suit = Hearts; rank = 12 } (* Queen of Hearts *)

let terminal_state : game_state =
  { trick =
      [ { suit = Clubs; rank = 14 }, North
      ; { suit = Clubs; rank = 5 }, East
      ; { suit = Clubs; rank = 9 }, South
      ; { suit = Hearts; rank = 12 }, West
      ]
  ; hearts_broken = true
  ; trick_number = 13
  ; target_score = 100
  ; decision = Winner North (* North wins the trick (highest club played) *)
  }
;;