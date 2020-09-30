let rec replicate element times list = 
    if times = 0 then list
    else
        element::(replicate element (times-1) list);;

let rep list n =
    let f = fun el acc ->
        replicate el n acc in
    List.fold_right f list [];;

rep [1;2;3;4] 3;;
