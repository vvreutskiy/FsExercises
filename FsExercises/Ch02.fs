namespace FsExercises

open System

module Ch02 = 
    let e01 x = 
        match x with
        | n when (x % 2 = 0 && x % 5 <> 0) -> true
        | n when (x % 3 = 0 && x % 5 <> 0) -> true
        | _ -> false
    
    let e02 (s, i) = 
        let rec conc s times acc = 
            match times with
            | 0 -> acc
            | n -> conc s (times - 1) (acc + s)
        conc (s) (i) ""
    
    let e03 (s : string, i, c : char) = s.[i] = c
    
    let e04 (s : string, i, c) = 
        let isChar e a = e = a
        
        let rec loop (str : string) (acc : int) (chr : char) = 
            match str with
            | "" -> acc
            | s when s.[0] = chr -> loop (s.[1..]) (acc + 1) chr
            | _ -> loop str.[1..] acc chr
        if i > s.Length then 0
        else loop (s.[i..]) 0 c
    
    let e05 (s, c) = e04 (s, 0, c)
    
    let e06 (d, n) = 
        match n % d with
        | 0 -> false
        | _ -> true
    
    let e0701 (a, b, c) = 
        if a > b then failwith "Argument exception"
        let rec loop divisor max number = 
            match divisor, max, number with
            | d, m, n when d = m -> 
                if e06 (d, n) then true
                else false
            | d, m, n when e06 (d, n) = false -> false
            | d, m, n -> loop (d + 1) m n
        loop a b c
    
    let e0702 n = e0701 (2, n - 1, n)
    
    let rec e0703 n = 
        if e0702 (n + 1) then n + 1
        else e0703 (n + 1)
    
    let rec e08 (n, k) = 
        match n, k with
        | 0, _ -> 1
        | _, 0 -> 1
        | n, k when n = k -> 1
        | n, k -> e08 (n - 1, k - 1) + e08 (n - 1, k)
    
    let rec e09 = 
        function 
        | (0, y) -> y
        | (x, y) -> e09 (x - 1, x * y)
    
    let e10 (c, e) = 
        if c then e
        else 0
    
    let e1101 n x = 
        let fn = float n
        fn + fn * (x / 100.0)
    
    let e1102 n x = 
        let fn = float n
        100.0 * (x - fn) / fn
    
    let e12 f = 
        let rec loop num f = 
            match num, f num with
            | 1000, _ -> failwith "none!"
            | n, 0 -> n
            | n, m -> loop (n + 1) f
        loop 0 f
    
    //    type FType<'a, 'b, 'c> = 'a * 'b -> 'c
    //    
    //    type GType<'a, 'b, 'c> = 'a -> 'b -> 'c
    //    
    //    type Curry<'a, 'b, 'c> = FType<'a, 'b, 'c> -> GType<'a, 'b, 'c>
    //    
    //    let e1301 : Curry<'a, 'b, 'c> = 
    //        fun (f : FType<'a, 'b, 'c>) (a : 'a) (b : 'b) -> 
    //            let newg (x : 'a) (y : 'b) = f (x, y)
    //            newg a b
    //    
    //    type Uncurry<'a, 'b, 'c> = GType<'a, 'b, 'c> -> FType<'a, 'b, 'c>
    //    
    //    let e1302 : Uncurry<'a, 'b, 'c> = 
    //        fun (g : GType<'a, 'b, 'c>) (a : 'a, b : 'b) -> 
    //            let newf (x : 'a, y : 'b) = g x y
    //            newf (a, b)
    let e1301 f = fun a b -> f (a, b)
    let e1302 g = fun (a, b) -> g a b
