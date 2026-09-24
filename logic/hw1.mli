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

val players : player_kind list
val suits : suit list
val ranks : rank list
val standard_deck : card list
val initial_state : game_state
val queen_of_spades : card
val ten_of_hearts : card
val example_hand : hand
val move_to_start_trick : move
val state_after_move_to_start_trick : game_state
