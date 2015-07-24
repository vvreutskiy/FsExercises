namespace FsExercises

open System

module Ch04 = 
    let e0101 num = [ 1..num ]
    
    let e0102 num = 
        let rec loop lst n = 
            if n = 0 then lst
            else loop (n :: lst) (n - 1)
        loop [] num
    
    let e0103 num = 
        Seq.unfold (fun n -> 
            if n > num then None
            else Some(n, n + 1)) 1
        |> List.ofSeq
    
    let e0201 num = [ num..(-1)..1 ]
    
    let e0202 num = 
        let rec loop lst n max = 
            if n > max then lst
            else loop (n :: lst) (n + 1) max
        loop [] 1 num
    
    let e0203 num = 
        num
        |> Seq.unfold (fun n -> 
               if n = 0 then None
               else Some(n, n - 1))
        |> List.ofSeq
    
    let e03 n = e0101 n |> List.filter (fun x -> x % 2 = 0)
    
    let rec e04 lst = 
        match lst, List.length lst with
        | _, 0 -> 0
        | x :: xs, n -> x * (int (-1. ** (float n - 1.0))) + (e04 xs)
    
    let rec e05 = 
        function 
        | [] -> []
        | [ x ] -> [ x ]
        | x0 :: x1 :: xs -> x0 :: e05 xs
    
    let rec e06 = 
        function 
        | [] -> []
        | x :: xs when x % 2 = 0 -> e06 xs
        | x :: xs -> x :: e06 xs
    
    let e07 elem lst = 
        let rec loop lst' acc = 
            match lst' with
            | [] -> acc
            | x :: xs when x = elem -> loop xs (acc + 1)
            | x :: xs -> loop xs acc
        loop lst 0
    
    let e08 lst = 
        let rec loop src left right = 
            match src with
            | [] -> (left, right)
            | x0 :: x1 :: xs -> loop xs (x0 :: left) (x1 :: right)
            | [ x ] -> (x :: left, right)
        
        let r, l = loop (List.rev lst) [] []
        l, r
    
    let e09 (left : 'a list, right : 'a list) = 
        let rec loop acc l r = 
            match l, r with
            | [], [] -> acc
            | (l :: ls, r :: rs) -> loop (r :: l :: acc) ls rs
            | _ -> failwith "left and right lists are not of equal length"
        List.rev (loop [] left right)
    
    let rec e10 (pref : 'a list) (lst : 'a list) = 
        match pref, lst with
        | [], _ -> true
        | p :: ps, l :: ls when p = l -> e10 ps ls
        | _ -> false
    
    let e1101 (lst, el) = 
        let rec loop lst' found = 
            match lst' with
            | [] -> found
            | x :: xs when x < el -> loop xs found
            | x :: xs when x = el -> loop xs (found + 1)
            | x :: xs when x > el -> found
        loop lst 0
    
    let e1102 (lst, el) = 
        let rec loop left right = 
            match right with
            | [] -> left @ right @ [ el ]
            | x :: xs when x < el -> loop (left @ [ x ]) xs
            | x :: xs when x >= el -> left @ [ el ] @ right
        loop [] lst
