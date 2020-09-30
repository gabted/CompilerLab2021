let explode string = 
    let rec help position list = 
        if position = String.length string
            then list
        else
            string.[position] :: (help (position+1) list)
    in help 0 [];;

let makeInt char = int_of_char char - 48;;

let sum a b = a+b;;

let rec superdigit n =
    let s = string_of_int n in
    if String.length s = 1
        then n
    else
        let list = explode s in
        let list = List.map makeInt list in
        let result = List.fold_left sum 0 list in
        superdigit result;;

