namespace FsExercisesTests

open FsExercises
open Xunit
open Swensen.Unquote
open Swensen.Unquote.Extensions

module Ch03tests = 
    open Ch03

    [<Fact>]
    let ``e0101 test``() =
        test <@ (11, 59, "AM") <* (1, 15, "PM") = true @>
        test <@ (11, 59, "AM") <* (11, 59, "AM") = false @>

    [<Fact>]
    let ``e0102 test``() =
        test <@ {Hours = 11; Minutes = 59; ToD = "AM"} <+ {Hours = 1; Minutes = 15; ToD = "PM"} = true @>
        test <@ {Hours = 11; Minutes = 59; ToD = "AM"} <+ {Hours = 11; Minutes = 59; ToD = "AM"} = false @>

    [<Fact>]
    let ``e0201 test``() = 
        test <@ (0, 5, 11) +* (1, 16, 10) = (2, 2, 9) @>

    [<Fact>]
    let ``e0202 test``() = 
        test <@ {Pounds = 0; Shillings = 5; Pence = 11} ++ {Pounds = 1; Shillings = 16; Pence = 10} = {Pounds = 2; Shillings = 2; Pence = 9} @>

    [<Fact>]
    let ``e0301 test``() =
        test <@ (2.0, -3.0) +. (-5.0, 4.0) = (-3.0, 1.0) @>

    [<Fact>]
    let ``e0302 test``() =
        test <@ (2.0, -3.0) *. (-5.0, 4.0) = (2.0, 23.0) @>

    [<Fact>]
    let ``e0303 test``() =
        test <@ (2.0, -3.0) -. (-5.0, 4.0) = (7.0, -7.0) @>

    [<Fact>]
    let ``e0304 test``() =
        let a, b = (2.0, -3.0) /. (-5.0, 4.0)
        Assert.Equal (a, -0.5366, 4)
        Assert.Equal (b, 0.1707, 4)

    [<Fact>]
    let ``e04 test``() = 
        test <@ mirrorVert (2., 1.) = (-2., -1.) @>
        test <@ mirrorHoriz (2., 1.) = (-2., 1.) @>
        test <@ printLine (5., -4.) = "y = 5.0x - 4.0" @>

    [<Fact>]
    let ``e05 test``() = 
        test <@ solve (1., -6., 9.) = One 3.0 @>
        test <@ solve (1., 2., -3.) = Two (-3., 1.) @>
        test <@ solve (1., 0., 10.) = None @>

    [<Fact>]
    let ``e06 test``() =
        test <@ (11, 59, Am) +<+ (1, 15, Pm) = true @>
        test <@ (11, 59, Am) +<+ (11, 59, Am) = false @>