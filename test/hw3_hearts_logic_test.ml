open! Core
open Hearts_logic_library.Hw2_hearts_logic

let ok_exn result = Result.ok result |> Option.value_exn

let%test "Example of a unit test (returns bool)" =
  let state = Game_state.create ~target_score:100 |> ok_exn in
  let expected_state : Game_state.t =
    { trick = []
    ; hearts_broken = false
    ; trick_number = 1
    ; target_score = 100
    ; decision = In_progress { whose_turn = North }
    ; last_move = None
    }
  in
  Game_state.equal state expected_state
;;

let create_and_print ~target_score =
  let result = Game_state.create ~target_score in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t list) Result.t)]
;;

let%expect_test "Example of an expect_test (returns unit)" =
  create_and_print ~target_score:100;
  [%expect
    {|
    (Ok
     ((trick ()) (hearts_broken false) (trick_number 1) (target_score 100)
      (decision (In_progress (whose_turn North))) (last_move ())))
    |}]
;;

let%expect_test "Game_state.create fails on invalid target score" =
  create_and_print ~target_score:0;
  [%expect {| (Error (Invalid_target_score)) |}];
  create_and_print ~target_score:(-10);
  [%expect {| (Error (Invalid_target_score)) |}]
;;

let make_move_and_print game_state card =
  let result = Game_state.make_move game_state card in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

let initial_game =
  Game_state.create ~target_score:100 |> ok_exn
;;

let%expect_test "Game_state.make_move first card of the trick" =
  make_move_and_print initial_game { suit = Clubs; rank = 2 };
  [%expect
    {|
    (Ok
     ((trick ((((suit Clubs) (rank 2)) North))) (hearts_broken false)
      (trick_number 1) (target_score 100)
      (decision (In_progress (whose_turn East)))
      (last_move (((suit Clubs) (rank 2))))))
    |}]
;;

let pretty_print_state ({ trick; trick_number; hearts_broken; decision; _ } : Game_state.t) =
  print_endline (sprintf "Trick: %d | Hearts broken: %b" trick_number hearts_broken);
  List.iter trick ~f:(fun (card, player) ->
    let player_str = Player_kind.sexp_of_t player |> Sexp.to_string in
    let suit_str = Suit.sexp_of_t card.suit |> Sexp.to_string in
    printf "%s played %s %d\n" player_str suit_str card.rank);
  print_s [%sexp (decision : Decision.t)]
;;

let print_final_state game_state cards =
  let result =
    List.fold cards ~init:game_state ~f:(fun new_state card ->
      Game_state.make_move new_state card |> ok_exn)
  in
  pretty_print_state result
;;

let%expect_test "Game_state.make_move: playing a full trick resolves and increments trick number" =
  print_final_state
    initial_game
    [ { suit = Clubs; rank = 2 }
    ; { suit = Clubs; rank = 5 }
    ; { suit = Clubs; rank = 14 }
    ; { suit = Clubs; rank = 9 }
    ];
  [%expect
    {|
    Trick: 2 | Hearts broken: false
    (In_progress (whose_turn North))
    |}]
;;

let%expect_test "Game_state.make_move: playing a heart breaks hearts" =
  print_final_state
    initial_game
    [ { suit = Hearts; rank = 4 } ];
  [%expect
    {|
    Trick: 1 | Hearts broken: true
    North played Hearts 4
    (In_progress (whose_turn East))
    |}]
;;

let%expect_test "Game_state.get_all_moves for Hearts" =
  let all_moves = Game_state.get_all_moves initial_game in
  print_endline (sprintf "Total distinct cards in deck: %d" (List.length all_moves));
  [%expect {| Total distinct cards in deck: 52 |}]
;;

let random_walk (initial_state : Game_state.t) ~random_seed =
  let rec random_walk (state : Game_state.t) =
    let all_moves = Game_state.get_all_moves state in
    let next_states =
      List.filter_map all_moves ~f:(fun move ->
        Game_state.make_move state move |> Result.ok)
    in
    let random_state = List.random_element next_states |> Option.value_exn in
    match Decision.is_game_over random_state.decision with
    | true -> random_state
    | false -> random_walk random_state
  in
  (* Set random seed. *)
  Core.Random.init random_seed;
  pretty_print_state (random_walk initial_state)
;;

let%expect_test "Hearts random walk till terminal state" =
  (* Note: Run `dune runtest -a` or `dune promote` to auto-fill the output block 
     for the 13-trick simulation. *)
  random_walk initial_game ~random_seed:1;
  [%expect {| |}];
;;