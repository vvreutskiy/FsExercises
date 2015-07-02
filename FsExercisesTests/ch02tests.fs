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
