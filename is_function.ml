let rec check_duplicate l = 
    let l = List.sort (fun a b -> a-b) l in
    let f acc curr = match acc with
        (n, false) -> (n, false)
        |(prev, true) -> (curr, prev<>curr)
        in
    let (_, res) = List.fold_left f (0, true) l in
    res;;

let is_function list =
    check_duplicate (List.map (fun (a, b) -> a) list);;

is_function [(1,2); (2,4); (3,6); (4,8); (1,0)];;