namespace FsExercisesTests

open FsExercises
open Xunit
open Swensen.Unquote
open Swensen.Unquote.Extensions

module Ch04tests = 
    open Ch04
    
    [<Fact>]
    let ``e0101 test``() = test <@ e0101 5 = [ 1; 2; 3; 4; 5 ] @>
    
    [<Fact>]
    let ``e0102 test``() = test <@ e0102 5 = [ 1; 2; 3; 4; 5 ] @>
    
    [<Fact>]
    let ``e0103 test``() = test <@ e0103 5 = [ 1; 2; 3; 4; 5 ] @>
    
    [<Fact>]
    let ``e0201 test``() = test <@ e0201 5 = [ 5; 4; 3; 2; 1 ] @>
    
    [<Fact>]
    let ``e0202 test``() = test <@ e0202 5 = [ 5; 4; 3; 2; 1 ] @>
    
    [<Fact>]
    let ``e0203 test``() = test <@ e0203 5 = [ 5; 4; 3; 2; 1 ] @>
    
    [<Fact>]
    let ``e03 test``() = test <@ e03 5 = [ 2; 4 ] @>
    
    [<Fact>]
    let ``e04 test``() = 
        test <@ e04 [ 1; 2; 3; 4; 5 ] = 1 - 2 + 3 - 4 + 5 @>
        test <@ e04 [ 2; -1; 3 ] = 6 @>
    
    [<Fact>]
    let ``e05 test``() = 
        test <@ e05 [ 0; 1; 2; 3; 4; 5 ] = [ 0; 2; 4 ] @>
        test <@ e05 [ 0; 1; 2; 3; 4; 5; 6 ] = [ 0; 2; 4; 6 ] @>
    
    [<Fact>]
    let ``e06 test``() = e06 [ 0; 1; 2; 3; 4; 5 ] =! [ 1; 3; 5 ]
    
    [<Fact>]
    let ``e07 test``() = 
        test <@ e07 1 [ 1; 2; 1; 1; 2; 3 ] = 3 @>
        test <@ e07 true [ true; false; false; true ] = 2 @>
    
    [<Fact>]
    let ``e08 test``() = test <@ e08 [ 1; 2; 3; 4; 5; 6 ] = ([ 1; 3; 5 ], [ 2; 4; 6 ]) @>
    
    [<Fact>]
    let ``e09 test``() = test <@ e09 ([ 1; 3; 5; 7 ], [ 2; 4; 6; 8 ]) = [ 1; 2; 3; 4; 5; 6; 7; 8 ] @>
    
    [<Fact>]
    let ``e10 test``() = 
        test <@ e10 [ 1; 2; 3 ] [ 1; 2; 3; 4; 5 ] = true @>
        test <@ e10 [ 1; 2; 3 ] [ 1; 2; 4; 5 ] = false @>
    
    [<Fact>]
    let ``e1101 test``() = e1101 ([ 1; 2; 2; 3; 3; 3; 3; 5; 6 ], 3) =! 4
    
    [<Fact>]
    let ``e1102 test``() = 
        test <@ e1102 ([ 1; 2; 3; 5; 6 ], 4) = [ 1; 2; 3; 4; 5; 6 ] @>
        test <@ e1102 ([ 1; 2; 3; 4; 5; 6 ], 4) = [ 1; 2; 3; 4; 4; 5; 6 ] @>

    [<Fact>]
    let ``e1103 test``() = test <@ false @>