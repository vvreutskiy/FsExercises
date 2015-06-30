namespace FsExercises

open System

module Ch01 = 
    let e01 (x : int) : int = x + 4
    
    let e02 (x : float * float) : float = 
        let first = fst x
        let second = snd x
        Math.Sqrt(first * first + second * second)
    
    let e0301 : int -> int = fun x -> x + 4
    
    let e0302 : float * float -> float = 
        fun x -> 
            match x with
            | first, second -> Math.Sqrt(first * first + second * second)
    
    let rec e04 x = 
        match x with
        | 0 -> 0
        | n -> n + e04 (n - 1)
    
    let rec e05 x = 
        match x with
        | 0 -> 0
        | 1 -> 1
        | n -> n + e05 (n - 1)
    
    let rec e06 x = 
        match x with
        | m, 0 -> m
        | m, n -> m + n + e06 (m, n - 1)
    
    type e07() = 
        let rec fact = 
            function 
            | 0 -> 1
            | n when n < 0 -> 0
            | n -> n * fact (n - 1)
        
        let rec power = 
            function 
            | (x, 0) -> 1.0
            | (x, n) -> x * power (x, n - 1)
        
        member x.x01() = ((System.Math.PI, fact -1), "(float * int)")
        member x.x02() = (fact (fact 4), "int")
        member x.x03() = (power (System.Math.PI, fact 2), "float")
        member x.x04() = ((power, fact), "((float * int -> float) * (int -> int))")

    let e08() =
        let a = 5
        let f a = a + 1
        let g b = (f b) + a
        (f 3), (g 3)