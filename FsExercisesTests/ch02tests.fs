namespace FsExercisesTests

open FsExercises
open Xunit
open Swensen.Unquote
open Swensen.Unquote.Extensions

module Ch02tests = 
    open Ch02
    
    [<Fact>]
    let ``e01 test``() = 
        test <@ e01 24 = true @>
        test <@ e01 27 = true @>
        test <@ e01 29 = false @>
        test <@ e01 30 = false @>
    
    [<Fact>]
    let ``e02 test``() = test <@ e02 ("123", 3) = "123123123" @>
    
    [<Fact>]
    let ``e03 test``() = 
        test <@ e03 ("123", 1, '2') = true @>
        test <@ e03 ("123", 1, 'z') = false @>
    
    [<Fact>]
    let ``e04 test``() = 
        test <@ e04 ("123123123", 3, '1') = 2 @>
        test <@ e04 ("123123123", 20, '1') = 0 @>
    
    [<Fact>]
    let ``e05 test``() = 
        test <@ e05 ("123123123", '1') = 3 @>
        test <@ e05 ("123123123", '0') = 0 @>
    
    [<Fact>]
    let ``e06 test``() = 
        test <@ e06 (3, 4) = true @>
        test <@ e06 (3, 6) = false @>
    
    [<Fact>]
    let ``e0701 test``() = 
        test <@ e0701 (3, 5, 7) = true @>
        test <@ e0701 (3, 5, 8) = false @>
    
    [<Fact>]
    let ``e0702 test``() = 
        test <@ e0702 11 = true @>
        test <@ e0702 12 = false @>
    
    [<Fact>]
    let ``e0703 test``() = 
        test <@ e0703 10 = 11 @>
        test <@ e0703 11 = 13 @>
    
    [<Fact>]
    let ``e08 test``() = 
        test <@ e08 (4, 2) = 6 @>
        test <@ e08 (0, 0) = 1 @>
        test <@ e08 (3, 1) = 3 @>
        test <@ e08 (6, 3) = 20 @>
    
    [<Fact>]
    let ``e09 test``() = 
        test <@ getSignature e09 = "(int * int -> int)" @>
        test <@ e09 (0, 1) = 1 @>
        test <@ e09 (5, 1) = 120 @>
    
    [<Fact>]
    let ``e10 test``() = test <@ getSignature e10 = "(bool * int -> int)" @>
    
    [<Fact>]
    let ``e11 test``() = 
        test <@ e1101 5 50.0 = 7.5 @>
        test <@ e1102 5 (e1101 5 50.0) = 50.0 @>

    [<Fact>]
    let ``e12 test``() =
        test <@ e12 (fun x -> if x = 5 then 0 else 1) = 5 @>
        test <@ e12 id = 0 @>
        raises<exn> <@ e12 (fun _ -> 1) = 0 @>

    [<Fact>]
    let ``e13 test``() =
        let f (x:int, y:float) = sprintf "int:%d float:%f" x y
        let g (x:int) (y:float) = sprintf "int:%d float:%f" x y
        test <@ f (1, 2.0) = "int:1 float:2.000000" @>
        test <@ g 1 2.0 = "int:1 float:2.000000" @>
        test <@ (e1301 f) 1 2.0 = g 1 2.0 && g 1 2.0 = "int:1 float:2.000000" @>
        test <@ (e1302 g) (1, 2.0) = f (1, 2.0) && f (1, 2.0) = "int:1 float:2.000000" @>