let rec gcd x y =
    if x > y then gcd (x - y) y
    else if x < y then gcd x (y - x)
    else x;;

*IFFY*
let gcd x y =
    let rec foo a b =
        if (a mod b == 0) then b else (let temp = a - b in foo (max temp b) (min temp b))

in foo (max x y) (min x y);
