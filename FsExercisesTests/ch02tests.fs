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
        test <@ false @>

