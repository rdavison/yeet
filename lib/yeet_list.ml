let permutations_recursive lst =
  let rec loop lst =
    if List.length lst < 2
    then [ lst ]
    else (
      let res = ref [] in
      let lst' = ref lst in
      for _ = 1 to List.length !lst' do
        match !lst' with
        | [] -> assert false
        | n :: rest ->
          let perms = loop rest |> List.map ~f:(fun p -> n :: p) in
          res := !res @ perms;
          lst' := rest @ [ n ]
      done;
      !res)
  in
  loop lst
;;

let%expect_test _ =
  let input = [ 1; 2; 3 ] in
  let output = permutations_recursive input in
  print_s ([%sexp_of: int list list] output);
  [%expect {| ((1 2 3) (1 3 2) (2 3 1) (2 1 3) (3 1 2) (3 2 1)) |}]
;;
