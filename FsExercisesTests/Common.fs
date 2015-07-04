namespace FsExercisesTests

[<AutoOpen>]
module Common =
    open Swensen.Unquote.Extensions
    
    let getSignature x : string = 
        let fullType = (x, 0).GetType().FSharpName
        fullType.[0..(fullType.Length - 7)]

module CommonTests =
    open Xunit
    open Swensen.Unquote

    [<Fact>]
    let ``getSignature test``() = 
        let a x = x + 1
        printfn "%A" ((a, 0).GetType().FSharpName)
        test <@ getSignature a = "(int -> int)" @>
        printfn "%A" ((1.0, 0).GetType().FSharpName)
        test <@ getSignature 1.0 = "float" @>
        printfn "%A" (([4], 0).GetType().FSharpName)
        test <@ getSignature [4] = "list<int>" @>