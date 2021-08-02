let rec find less root want = match root with
| Nulltree -> None
| Node (key, value, left, right) ->
    if less want key then find less left want
    else if less key want then find less right want
    else Some value;;




let rec find2 less root want = match root with
| Nulltree -> None
| Node (key, _, left, _) when less want key -> find2 less left want
| Node (key, _, _, right) when less key want -> find2 less right want
| Node (_, value, _, _) -> Some value;;
