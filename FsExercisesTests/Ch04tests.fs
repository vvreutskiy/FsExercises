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
