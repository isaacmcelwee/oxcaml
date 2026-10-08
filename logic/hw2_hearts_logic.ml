open! Core

module Player_kind = struct
  type t =
    | North
    | East
    | South
    | West
  [@@deriving sexp, compare, equal]

  let next (t : t) : t =
    match t with
    | North -> East
    | East -> South
    | South -> West
    | West -> North
  ;;
end

module Suit = struct
  type t =
    | Clubs
    | Diamonds
    | Spades
    | Hearts
  [@@deriving sexp, compare, equal]
end

module Card = struct
  module T = struct
    type t =
      { suit : Suit.t
      ; rank : int
      }
    [@@deriving sexp, compare]
  end

  include T

  (* Creates a [Card.Map.t] and Set.t *)
  include Comparable.Make (T)
end

module Move = Card

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, compare, equal]

  let is_game_over t =
    match t with
    | Stalemate | Winner _ -> true
    | In_progress _ -> false
  ;;
end

module Game_state = struct
  type t =
    { trick : (Card.t * Player_kind.t) list
    ; hearts_broken : bool
    ; trick_number : int
    ; target_score : int
    ; decision : Decision.t
    ; last_move : Move.t option (* For animation purposes. *)
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t = Invalid_target_score
    [@@deriving sexp, compare]
  end

  let create ~target_score : (t, Create_error.t list) Result.t =
    if target_score > 0
    then
      Ok
        { trick = []
        ; hearts_broken = false
        ; trick_number = 1
        ; target_score
        ; decision = In_progress { whose_turn = North }
        ; last_move = None
        }
    else Error [ Create_error.Invalid_target_score ]
  ;;

  (* Simplistic trick evaluator: whoever played the first card wins 
     (in a real Hearts game, this would check highest rank of the led suit) *)
  let evaluate_trick_winner (trick : (Card.t * Player_kind.t) list) =
    match List.hd trick with
    | Some (_, player) -> player
    | None -> North
  ;;

  module Move_error = struct
    type t =
      | Game_is_over
      | Card_already_played
      | Illegal_card
    [@@deriving sexp, compare]
  end

  let get_all_moves _t : Move.t list =
    let suits = [ Suit.Clubs; Diamonds; Spades; Hearts ] in
    let ranks = List.range 2 15 in (* 2 through 14 (Ace) *)
    List.cartesian_product suits ranks
    |> List.map ~f:(fun (suit, rank) : Move.t -> { suit; rank })
  ;;

  let make_move t (card : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Winner _ | Stalemate -> Error Game_is_over
    | In_progress { whose_turn } ->
      (* Simplified Hearts move logic *)
      let new_trick_list = t.trick @ [ card, whose_turn ] in
      let hearts_broken = t.hearts_broken || match card.suit with | Hearts -> true | _ -> false in

      if List.length new_trick_list < 4 then
        Ok
          { t with 
            trick = new_trick_list
          ; hearts_broken
          ; decision = In_progress { whose_turn = Player_kind.next whose_turn }
          ; last_move = Some card 
          }
      else
        let trick_winner = evaluate_trick_winner new_trick_list in
        let decision : Decision.t =
          if t.trick_number >= 13 then
            Winner trick_winner (* Simplified: trick 13 winner wins game *)
          else
            In_progress { whose_turn = trick_winner }
        in
        Ok 
          { t with 
            trick = []
          ; trick_number = t.trick_number + 1
          ; hearts_broken
          ; decision
          ; last_move = Some card 
          }
  ;;

  module For_testing = struct
    let evaluate_trick_winner = evaluate_trick_winner
  end
end