open! Core

type player_kind =
  | North
  | East
  | South
  | West

type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card =
  { rank : rank
  ; suit : suit
  }

type hand =
  { player : player_kind
  ; cards : card list
  }

type trick = (player_kind * card) list

type phase =
  | Passing
  | Playing
  | Round_complete

type decision =
  | In_progress of { phase : phase; whose_turn : player_kind }
  | Game_over of { winner : player_kind }

type game_state =
  { hands : hand list
  ; current_trick : trick
  ; completed_tricks : trick list
  ; scores : (player_kind * int) list
  ; hearts_broken : bool
  ; decision : decision
  }

type move =
  | Pass_cards of { player : player_kind; cards : card list }
  | Play_card of { player : player_kind; card : card }

let players = [ North; East; South; West ]

let suits = [ Clubs; Diamonds; Hearts; Spades ]

let ranks =
  [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

let standard_deck : card list =
  List.concat_map suits ~f:(fun suit -> List.map ranks ~f:(fun rank -> { rank; suit }))
;;

let initial_state : game_state =
  { hands = List.map players ~f:(fun player -> { player; cards = [] })
  ; current_trick = []
  ; completed_tricks = []
  ; scores = List.map players ~f:(fun player -> player, 0)
  ; hearts_broken = false
  ; decision = In_progress { phase = Passing; whose_turn = North }
  }
;;

let queen_of_spades : card = { rank = Queen; suit = Spades }

let ten_of_hearts : card = { rank = Ten; suit = Hearts }

let example_hand : hand =
  { player = North
  ; cards = [ { rank = Two; suit = Clubs }; ten_of_hearts; queen_of_spades ]
  }
;;

let move_to_start_trick : move =
  Play_card { player = North; card = { rank = Two; suit = Clubs } }
;;

let state_after_move_to_start_trick : game_state =
  { initial_state with
    hands = [ example_hand ]
  ; current_trick = [ North, { rank = Two; suit = Clubs } ]
  ; decision = In_progress { phase = Playing; whose_turn = East }
  }
;;
