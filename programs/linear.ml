let rec find cmp key list = match list with
    | [] -> None
    | (k,v)::xs -> if cmp key k then Some v
                   else find cmp key xs;;
