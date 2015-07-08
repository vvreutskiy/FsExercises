namespace FsExercises

open System

module Ch03 = 
    type TheDateTup = int * int * string
    
    let (<*) (d1 : TheDateTup) (d2 : TheDateTup) : bool = 
        let h1, m1, tod1 = d1
        let h2, m2, tod2 = d2
        match tod1, tod2 with
        | "AM", "PM" -> true
        | "PM", "AM" -> false
        | _ -> (h1, m1) < (h2, m2)
    
    type TheDateRec = 
        { Hours : int
          Minutes : int
          ToD : string }
    
    let (<+) (d1 : TheDateRec) (d2 : TheDateRec) = (d1.Hours, d1.Minutes, d1.ToD) <* (d2.Hours, d2.Minutes, d2.ToD)
    
    type BritCurTup = int * int * int
    
    let (+*) (c1 : BritCurTup) (c2 : BritCurTup) : BritCurTup = 
        let po1, sh1, pe1 = c1
        let po2, sh2, pe2 = c2
        let peResult = (pe1 + pe2) % 12
        let peRemain = (pe1 + pe2) / 12
        let shResult = (sh1 + sh2 + peRemain) % 20
        let shRemain = (sh1 + sh2 + peRemain) / 20
        (po1 + po2 + shRemain, shResult, peResult)
    
    type BritCurRec = 
        { Pounds : int
          Shillings : int
          Pence : int }
    
    let (++) (c1 : BritCurRec) (c2 : BritCurRec) : BritCurRec = 
        let peSum = c1.Pence + c2.Pence
        let peResult = peSum % 12
        let peRemain = peSum / 12
        let shSum = c1.Shillings + c2.Shillings + peRemain
        let shResult = shSum % 20
        let shRemain = shSum / 20
        { Pounds = c1.Pounds + c2.Pounds + shRemain
          Shillings = shResult
          Pence = peResult }
    
    type Cpx = float * float
    
    let (+.) (c1 : Cpx) (c2 : Cpx) : Cpx = 
        let a, b = c1
        let c, d = c2
        (a + c, b + d)
    
    let ( *. ) (c1 : Cpx) (c2 : Cpx) : Cpx = 
        let a, b = c1
        let c, d = c2
        (a * c - b * d, b * c + a * d)
    
    let neg (c : Cpx) : Cpx = 
        let a, b = c
        (-a, -b)
    
    let rev (c : Cpx) : Cpx = 
        let a, b = c
        if a = 0.0 && b = 0.0 then failwith "Division by zero"
        else 
            let a2b2 = a ** 2.0 + b ** 2.0
            (a / a2b2, -b / a2b2)
    
    let (-.) (c1 : Cpx) (c2 : Cpx) : Cpx = c1 +. (neg c2)
    let (/.) (c1 : Cpx) (c2 : Cpx) : Cpx = c1 *. (rev c2)
    
    type Line = float * float
    
    let mirrorVert (l : Line) : Line = 
        let a, b = l
        (-a, -b)
    
    let mirrorHoriz (l : Line) : Line = 
        let a, b = l
        (-a, b)
    
    let printLine (l : Line) : string = 
        let a, b = l
        sprintf "y = %.1fx %s %.1f" a (if b < 0.0 then "-"
                                       else "+") (Math.Abs b)
    
    type Solution = 
        | Two of first : float * second : float
        | One of float
        | None
    
    let solve (a, b, c) = 
        let dt = b ** 2.0 - 4.0 * a * c
        let rt1 a b d = -(b + Math.Sqrt(d)) / (2.0 * a)
        let rt2 a b d = -(b - Math.Sqrt(d)) / (2.0 * a)
        match dt with
        | 0.0 -> One(-b / (2.0 * a))
        | _ when dt > 0.0 -> Two(rt1 a b dt, rt2 a b dt)
        | _ when dt < 0.0 -> None
    
    type ToD = 
        | Am
        | Pm
    
    type TheDateTag = int * int * ToD
    
    let (+<+) (d1 : TheDateTag) (d2 : TheDateTag) : bool = 
        let h1, m1, tod1 = d1
        let h2, m2, tod2 = d2
        match tod1, tod2 with
        | Am, Pm -> true
        | Pm, Am -> false
        | _ -> (h1, m1) < (h2, m2)
    
    type Shape = 
        | Circle of float
        | Square of float
        | Triangle of float * float * float
    
    let isShape = 
        function 
        | Circle r -> r > 0.0
        | Square a -> a > 0.0
        | Triangle(a, b, c) -> a > 0.0 && b > 0.0 && c > 0.0 && a < b + c && b < c + a && c < a + b
    
    let area x = 
        match x with
        | x' when isShape x -> 
            match x with
            | Circle r -> System.Math.PI * r * r
            | Square a -> a * a
            | Triangle(a, b, c) -> 
                let s = (a + b + c) / 2.0
                sqrt (s * (s - a) * (s - b) * (s - c))
        | _ -> failwith "not a legal shape" raise
