let fact n =
    let rec help result n = match (result, n) with
        (x, 0.) -> x
        |(x, n) -> help (x*.n) (n-.1.)
    in help 1. n;;

let myExp n = 
    let rec help result i =     
        if i = 20. 
            then result
        else
            let newTerm = (n**i)/.(fact i) in
            print_endline ((string_of_float i)^" "^(string_of_float newTerm));
            help (result +. newTerm) (i +. 1.)
            
    in help 0. 0. 
in myExp 2.;;