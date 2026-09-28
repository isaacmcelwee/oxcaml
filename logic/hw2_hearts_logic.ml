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

let game_over_score = 100

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

let rank_value rank =
  match rank with
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | Ten -> 10
  | Jack -> 11
  | Queen -> 12
  | King -> 13
  | Ace -> 14
;;

let winner_of_trick trick =
  match trick with
  | [] -> None
  | (leading_player, leading_card) :: cards ->
    let winning_player, _ =
      List.fold cards ~init:(leading_player, leading_card) ~f:(fun winning (player, card) ->
        let _, winning_card = winning in
        if Poly.equal card.suit winning_card.suit
           && rank_value card.rank > rank_value winning_card.rank
        then player, card
        else winning)
    in
    Some winning_player
;;

let points_in_trick trick =
  List.sum (module Int) trick ~f:(fun (_player, card) ->
    match card.suit, card.rank with
    | Hearts, _ -> 1
    | Spades, Queen -> 13
    | _ -> 0)
;;

let add_trick_points scores trick =
  match winner_of_trick trick with
  | None -> scores
  | Some winner ->
    List.map scores ~f:(fun (player, score) ->
      if Poly.equal player winner then player, score + points_in_trick trick else player, score)
;;

let round_is_complete state = List.length state.completed_tricks = 13

let game_winner state =
  match List.filter state.scores ~f:(fun (_player, score) -> score >= game_over_score) with
  | [] -> None
  | _ ->
    List.min_elt state.scores ~compare:(fun (_player, score) (_other_player, other_score) ->
      Int.compare score other_score)
    |> Option.map ~f:fst
;;
