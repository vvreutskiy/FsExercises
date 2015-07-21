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

    let rec e05 = function
        | [] -> []
        | [x] -> [x]
        | x0 :: x1 :: xs -> x0 :: e05 xs