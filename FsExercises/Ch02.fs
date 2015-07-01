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
    //String.replicate (snd v) (fst v)
    
    let e03 (v : string * int * char) : bool = 
        let s, i, c = v
        s.[i] = c
