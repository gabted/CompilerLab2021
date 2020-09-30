open List;;

let explode string = 
    let rec help position list = 
        if position = String.length string
            then list
        else
            string.[position] :: (help (position+1) list)
    in help 0 [];;

let rec merge l1 l2 = 
    match (l1, l2) with
    ([], []) -> []
    |(l, []) -> l
    |([], l) -> l
    |(hd::tl, l) when length l1 = length l2 -> hd::merge tl l2
    |(l, hd::tl) -> hd::merge l tl;;

let mingle s1 s2 = 
    merge (explode s1) (explode s2);;

mingle "abcde" "pqrst";;