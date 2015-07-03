namespace FsExercisesTests

open FsExercises
open Xunit
open Swensen.Unquote


module Ch01Tests = 
    open Ch01


    
    [<Fact>]
    let ``e01 test``() = test <@ e01 5 = 9 @>
    
    [<Fact>]
    let ``e02 test``() = test <@ e02 (3., 4.) = 5. @>
    
    [<Fact>]
    let ``e03 test``() = 
        test <@ e0301 5 = 9 @>
        test <@ e0302 (3., 4.) = 5. @>
    
    [<Fact>]
    let ``e04 test``() = test <@ e04 3 = 6 @>
    
    [<Fact>]
    let ``e05 test``() = test <@ e05 10 = 55 @>
    
    [<Fact>]
    let ``e06 test``() = test <@ e06 (4, 3) = 22 @>
    
    [<Fact>]
    let ``e0701 test``() = 
        let sut = new e07()
        
        let actualSignature = 
            sut.x01()
            |> fst
            |> getSignature
        
        let expectedSignature = (snd (sut.x01()))
        test <@ actualSignature = expectedSignature @>
    
    [<Fact>]
    let ``e0702 test``() = 
        let sut = new e07()
        
        let actualSignature = 
            sut.x02()
            |> fst
            |> getSignature
        
        let expectedSignature = (snd (sut.x02()))
        test <@ actualSignature = expectedSignature @>
    
    [<Fact>]
    let ``e0703 test``() = 
        let sut = new e07()
        
        let actualSignature = 
            sut.x03()
            |> fst
            |> getSignature
        
        let expectedSignature = (snd (sut.x03()))
        test <@ actualSignature = expectedSignature @>
    
    [<Fact>]
    let ``e0704 test``() = 
        let sut = new e07()
        
        let actualSignature = 
            sut.x04()
            |> fst
            |> getSignature
        
        let expectedSignature = (snd (sut.x04()))
        test <@ actualSignature = expectedSignature @>

    [<Fact>]
    let ``e08 test``() =
        let (x, y) = e08()
        test <@ x = 4 @>
        test <@ y = 9 @>