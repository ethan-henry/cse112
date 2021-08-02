let rec factorial num acc = match num with
| 0 -> acc
| _ -> factorial (num - 1) (acc * num);;
