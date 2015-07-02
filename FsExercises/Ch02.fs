namespace FsExercises

open System

module Ch02 = 
    let e01 (x : int) : bool = 
        match x with
        | n when (x % 2 = 0 && x % 5 <> 0) -> true
        | n when (x % 3 = 0 && x % 5 <> 0) -> true
        | _ -> false
    
    let e02 (v : string * int) : string = 
        let rec conc s times acc = 
            match times with
            | 0 -> acc
            | n -> conc s (times - 1) (acc + s)
        conc (fst v) (snd v) ""
    
    let e03 (v : string * int * char) : bool = 
        let s, i, c = v
        s.[i] = c
    
    let e04 (v : string * int * char) : int = 
        let (s, i, c) = v
        let isChar e a = e = a
        
        let rec loop (str : string) (acc : int) (chr : char) = 
            match str with
            | "" -> acc
            | s when s.[0] = chr -> loop (s.[1..]) (acc + 1) chr
            | _ -> loop str.[1..] acc chr
        if i > s.Length then 0
        else loop (s.[i..]) 0 c
    
    let e05 (v : string * char) = 
        let s, c = v
        e04 (s, 0, c)
    
    let e06 (v : int * int) = 
        let d, n = v
        match n % d with
        | 0 -> false
        | _ -> true
    
    let e0701 (v : int * int * int) : bool = 
        let a, b, c = v
        if a > b then failwith "Argument exception"
        [ a..b ] |> List.fold (fun acc el -> acc && e06 (el, c)) true
    
    let e0702 (n : int) : bool = e0701 (2, n - 1, n)
    
    let rec e0703 (n : int) : int = 
        if e0702 (n + 1) then n + 1
        else e0703 (n + 1)
