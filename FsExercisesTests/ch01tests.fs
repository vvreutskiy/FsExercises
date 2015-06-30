namespace FsExercisesTests

open FsExercises
open Xunit
open Swensen.Unquote
open Swensen.Unquote.Extensions

module Ch01Tests = 
    let getSignature x : string = 
        let fullType = (x, 0).GetType().FSharpName
        fullType.[0..(fullType.Length - 7)]
    
    [<Fact>]
    let ``getSignature test``() = 
        let a x = x + 1
        printfn "%A" ((a, 0).GetType().FSharpName)
        test <@ getSignature a = "(int -> int)" @>
        printfn "%A" ((1.0, 0).GetType().FSharpName)
        test <@ getSignature 1.0 = "float" @>
        printfn "%A" (([4], 0).GetType().FSharpName)
        test <@ getSignature [4] = "list<int>" @>
    
    [<Fact>]
    let ``e01 test``() = test <@ Ch01.e01 5 = 9 @>
    
    [<Fact>]
    let ``e02 test``() = test <@ Ch01.e02 (3., 4.) = 5. @>
    
    [<Fact>]
    let ``e03 test``() = 
        test <@ Ch01.e0301 5 = 9 @>
        test <@ Ch01.e0302 (3., 4.) = 5. @>
    
    [<Fact>]
    let ``e04 test``() = test <@ Ch01.e04 3 = 6 @>
    
    [<Fact>]
    let ``e05 test``() = test <@ Ch01.e05 10 = 55 @>
    
    [<Fact>]
    let ``e06 test``() = test <@ Ch01.e06 (4, 3) = 22 @>
    
    [<Fact>]
    let ``e0701 test``() = 
        let sut = new Ch01.e07()
        
        let actualSignature = 
            sut.x01()
            |> fst
            |> getSignature
        
        let expectedSignature = (snd (sut.x01()))
        test <@ actualSignature = expectedSignature @>
    
    [<Fact>]
    let ``e0702 test``() = 
        let sut = new Ch01.e07()
        
        let actualSignature = 
            sut.x02()
            |> fst
            |> getSignature
        
        let expectedSignature = (snd (sut.x02()))
        test <@ actualSignature = expectedSignature @>
    
    [<Fact>]
    let ``e0703 test``() = 
        let sut = new Ch01.e07()
        
        let actualSignature = 
            sut.x03()
            |> fst
            |> getSignature
        
        let expectedSignature = (snd (sut.x03()))
        test <@ actualSignature = expectedSignature @>
    
    [<Fact>]
    let ``e0704 test``() = 
        let sut = new Ch01.e07()
        
        let actualSignature = 
            sut.x04()
            |> fst
            |> getSignature
        
        let expectedSignature = (snd (sut.x04()))
        test <@ actualSignature = expectedSignature @>

    [<Fact>]
    let ``e08 test``() =
        let (x, y) = Ch01.e08()
        test <@ x = 4 @>
        test <@ y = 9 @>