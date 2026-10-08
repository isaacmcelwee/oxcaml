open! Core

module Player_kind : sig
  type t =
    | North
    | East
    | South
    | West
  [@@deriving sexp, compare, equal]

  val next : t -> t
end

module Suit : sig
  type t =
    | Clubs
    | Diamonds
    | Spades
    | Hearts
  [@@deriving sexp, compare, equal]
end

module Card : sig
  type t =
    { suit : Suit.t
    ; rank : int
    }
  [@@deriving sexp, compare]

  (* Defines a [Card.Map.t]. *)
  include Comparable.S with type t := t
end

module Move : module type of Card

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
    | Stalemate
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Game_state : sig
  type t =
    { trick : (Card.t * Player_kind.t) list
    ; hearts_broken : bool
    ; trick_number : int
    ; target_score : int
    ; decision : Decision.t
    ; last_move : Move.t option (* For animation purposes. *)
    }
  [@@deriving sexp, compare, equal]

  module Create_error : sig
    type t = 
      | Invalid_target_score
    [@@deriving sexp, compare]
  end

  val create
    :  target_score:int
    -> (t, Create_error.t list) Result.t

  module Move_error : sig
    type t =
      | Game_is_over
      | Card_already_played
      | Illegal_card
    [@@deriving sexp, compare]
  end

  val get_all_moves : t -> Move.t list
  val make_move : t -> Move.t -> (t, Move_error.t) Result.t

  module For_testing : sig
    val evaluate_trick_winner : (Card.t * Player_kind.t) list -> Player_kind.t
  end
end