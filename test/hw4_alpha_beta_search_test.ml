open! Core
open Hearts_logic_library.Hw2_hearts_logic
open Hearts_logic_library.Hw4_alpha_beta_search

let ok_exn result = Result.ok result |> Option.value_exn

let pretty_print_state (state : Game_state.t) =
  let trick = state.trick in
  let trick_number = state.trick_number in
  let hearts_broken = state.hearts_broken in
  let decision = state.decision in
  print_endline (sprintf "Trick: %d | Hearts broken: %b" trick_number hearts_broken);
  List.iter trick ~f:(fun (card, player) ->
    let player_str = Player_kind.sexp_of_t player |> Sexp.to_string in
    let suit_str = Suit.sexp_of_t card.suit |> Sexp.to_string in
    printf "%s played %s %d\n" player_str suit_str card.rank);
  print_s [%sexp (decision : Decision.t)]
;;

let print_computer_move (trick_cards : (Suit.t * int) list) max_depth =
  let trick =
    List.mapi trick_cards ~f:(fun i (suit, rank) ->
      let player =
        match i with
        | 0 -> Player_kind.North
        | 1 -> Player_kind.East
        | 2 -> Player_kind.South
        | 3 -> Player_kind.West
        | _ -> failwith "Trick full"
      in
      ({ Card.suit; rank }, player)
    )
  in
  let whose_turn =
    match List.length trick_cards with
    | 0 -> Player_kind.North
    | 1 -> Player_kind.East
    | 2 -> Player_kind.South
    | 3 -> Player_kind.West
    | _ -> failwith "Trick full"
  in
  let state : Game_state.t =
    { trick
    ; hearts_broken = false
    ; trick_number = 13 (* Trick 13 ends the game in our simplified engine *)
    ; target_score = 100
    ; decision = In_progress { whose_turn }
    ; last_move = None
    }
  in
  let move = alpha_beta state ~depth:max_depth |> Option.value_exn in
  let next_state = Game_state.make_move state move |> ok_exn in
  print_s [%message "Computer chooses this move" (move : Move.t)];
  print_endline "\nThis transitions the game from this state:";
  pretty_print_state state;
  print_endline "\nTo this state:";
  pretty_print_state next_state
;;

(* Note: Run `dune promote` to automatically populate the [%expect] blocks 
   with the output from your simplified engine. *)

let%expect_test "Computer plays first card of the trick" =
  print_computer_move [] 1;
  [%expect {| |}]
;;

let%expect_test "Computer responds to the first card played" =
  print_computer_move [ (Suit.Clubs, 2) ] 1;
  [%expect {| |}]
;;

let%expect_test "Computer plays the final card of the trick and resolves the game" =
  print_computer_move [ (Suit.Clubs, 2); (Suit.Clubs, 5); (Suit.Clubs, 14) ] 1;
  [%expect {| |}]
;;