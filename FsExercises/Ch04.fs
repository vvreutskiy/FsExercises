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
