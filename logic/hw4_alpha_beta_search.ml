open! Core
open Hw2_hearts_logic

(*1 if we win, -1 if they win, 0 otherwise *)
let heuristic_value (node : Game_state.t) ~(me : Player_kind.t) =
  match node.decision with
  | Winner winner -> if Player_kind.equal winner me then 1 else -1
  | _ -> 0
;;

(* 2. Get valid children *)
let children node =
  Game_state.get_all_moves node
  |> List.filter_map ~f:(fun move -> Game_state.make_move node move |> Result.ok)
;;

(* 3. Alpha-beta: "Me" (Maximize) vs "Everyone else" (Minimize) *)
let rec alpha_beta_value (node : Game_state.t) depth alpha beta ~(me : Player_kind.t) =
  match node.decision with
  | In_progress { whose_turn } when depth > 0 ->
    if Player_kind.equal whose_turn me then
      (* My turn: Maximize *)
      List.fold_until (children node) ~init:(Int.min_value, alpha)
        ~finish:(fun (v, _) -> v)
        ~f:(fun (v, alpha) child ->
          let child_score = alpha_beta_value child (depth - 1) alpha beta ~me in
          let v = Int.max v child_score in
          if v >= beta then Stop v else Continue (v, Int.max alpha v))
    else
      (* Opponents' turn *)
      List.fold_until (children node) ~init:(Int.max_value, beta)
        ~finish:(fun (v, _) -> v)
        ~f:(fun (v, beta) child ->
          let child_score = alpha_beta_value child (depth - 1) alpha beta ~me in
          let v = Int.min v child_score in
          if v <= alpha then Stop v else Continue (v, Int.min beta v))
  | _ -> heuristic_value node ~me
;;

let alpha_beta (node : Game_state.t) ~depth =
  match node.decision with
  | In_progress { whose_turn = me } ->
    Game_state.get_all_moves node
    |> List.filter_map ~f:(fun move ->
         match Game_state.make_move node move with
         | Error _ -> None
         | Ok child ->
           let score = alpha_beta_value child (depth - 1) Int.min_value Int.max_value ~me in
           Some (move, score))
    |> List.max_elt ~compare:(fun (_, score1) (_, score2) -> Int.compare score1 score2)
    |> Option.map ~f:fst
  | _ -> None
;;
