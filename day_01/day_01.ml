open Batteries

let rec from_stdin state =
    try
        from_stdin state @ [ read_int ()]
    with
    | _ -> state

let rec sliding n xs =
    if List.length xs >= n 
    then List.cons (BatList.take n xs) (sliding n (List.tl xs))
    else []

let increases lst =
    let windows = sliding 2 lst in
    let f = fun acc x ->
        match x with
        | a::b::[] -> if b > a then acc + 1 else acc
        | _ -> acc
    in
    List.fold_left f 0 windows

let main () =
    let values = BatList.rev @@ from_stdin [] in
    let sliding_sum = List.map List.sum (sliding 3 values) in
    print_string ("Small window: " ^ (string_of_int (increases values)) ^ "\n");
    print_string ("Large window: " ^ (string_of_int (increases sliding_sum)) ^ "\n")

let () = main ()
