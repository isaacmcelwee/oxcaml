open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic

module Hearts = Tictactoe_logic_library.Hw2_hearts_logic

let ok_exn result = Result.ok result |> Option.value_exn

let pretty_print_board ({ board; rows; columns; decision; _ } : Game_state.t) =
  let row_separator =
    List.range 0 columns |> List.map ~f:(fun _ -> "-") |> String.concat ~sep:"-"
  in
  for row = 0 to rows - 1 do
    List.range 0 columns
    |> List.map ~f:(fun column ->
      match Map.find board { row; column } with
      | None -> " "
      | Some player -> Player_kind.sexp_of_t player |> Sexp.to_string)
    |> String.concat ~sep:"|"
    |> print_endline;
    if row < rows - 1 then print_endline row_separator
  done;
  print_s [%sexp (decision : Decision.t)]
;;

let%test "the standard Hearts deck has 52 unique cards" =
  List.length Hearts.standard_deck = 52
  && List.length (List.dedup_and_sort Hearts.standard_deck ~compare:Poly.compare) = 52
;;

let%test "rank values increase from two through ace" =
  Hearts.rank_value Hearts.Two = 2 && Hearts.rank_value Hearts.Ace = 14
;;

let%test "only cards in the led suit can win a trick" =
  let trick =
    [ Hearts.North, { Hearts.rank = Hearts.Two; suit = Hearts.Clubs }
    ; Hearts.East, { Hearts.rank = Hearts.Ace; suit = Hearts.Hearts }
    ; Hearts.South, { Hearts.rank = Hearts.Three; suit = Hearts.Clubs }
    ; Hearts.West, { Hearts.rank = Hearts.Ace; suit = Hearts.Clubs }
    ]
  in
  Poly.equal (Hearts.winner_of_trick trick) (Some Hearts.West)
;;

let%test "hearts and the queen of spades score their standard points" =
  let trick =
    [ Hearts.North, Hearts.ten_of_hearts
    ; Hearts.East, Hearts.queen_of_spades
    ; Hearts.South, { Hearts.rank = Hearts.Two; suit = Hearts.Clubs }
    ]
  in
  Hearts.points_in_trick trick = 14
;;

let%test "trick points are awarded to the trick winner" =
  let trick =
    [ Hearts.North, { Hearts.rank = Hearts.Two; suit = Hearts.Clubs }
    ; Hearts.East, Hearts.queen_of_spades
    ; Hearts.South, { Hearts.rank = Hearts.Ace; suit = Hearts.Clubs }
    ]
  in
  let scores = List.map Hearts.players ~f:(fun player -> player, 0) in
  let scores = Hearts.add_trick_points scores trick in
  List.Assoc.find_exn scores Hearts.South ~equal:Poly.equal = 13
;;

let%test "a Hearts round is complete after thirteen tricks" =
  let state =
    { Hearts.initial_state with completed_tricks = List.init 13 ~f:(fun _ -> []) }
  in
  Hearts.round_is_complete state
;;

let%test "the lowest-scoring player wins once someone reaches 100" =
  let state =
    { Hearts.initial_state with
      scores = [ Hearts.North, 12; Hearts.East, 100; Hearts.South, 45; Hearts.West, 70 ]
    }
  in
  Poly.equal (Hearts.game_winner state) (Some Hearts.North)
;;