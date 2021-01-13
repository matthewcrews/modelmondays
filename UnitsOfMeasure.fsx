// What can you do

[<Measure>] type USD
[<Measure>] type cm
[<Measure>] type m
[<Measure>] type gm
[<Measure>] type kg

// You can also declare products of Units
[<Measure>] type N = kg / m^3

// F# Will enforce proper algebra
let x1 = 10.0<cm>
let x2 = 5.0<cm>
let x3 = x1 + x2 // This works because the units match

// Mismatched units will not compile
let y1 = 2.0<kg>
// let z1 = x1 + y1 // Compiler will complain about units

// Units will cancel where appropriate
let y2 = 1.0<N> // A float with complex units
let z3 = y2 * 1.0<m^3> // Multiplying will cancel the 1/m^3
let z4 = z3 + 1.0<kg>

// Units also work with other numeric types
let a1 = 1<cm>
let b1 = 10.0M<kg>

// F# does not have implicit casting though so
// you will need to convert the types. The downside is that
// this will strip the units off of the number
// let a2 = x1 + a1 // Will not work becuase numeric type is different

// Converting to float removes the units
let a2 = float a1


// The downsides of Units of Measure

// 1. They are not available for all types

// let aString = "A string"<cm>
// let aBool = true<cm>

// 2. Math functions don't understand them
open System
let aValue = 1.0<cm>
// let anAbs = Math.Abs aValue // Compiler is going to say no

// This may seem obnoxious but F# almost always defaults to the "safe"
// thing to do. Not all of the math functions behave well with 
// Units of Measure so it doesn't assume

// How to get around this?
// Wrappers :/

let myAbs (x: float<'Measure>) =
    (Math.Abs(float x)) 
    |> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure>

// Whether this is worth it to you, is up to you. I've collected enough
// at this point it's trivial.

// Let's talk about adding UoM to your types
// This example is something we would like to add UoM to

// type Vector = {
//     X : float
//     Y : float
//     Z : float
// }

// let add (a: Vector) (b: Vector) =
//     {
//         X = a.X + b.X
//         Y = a.Y + b.Y
//         Z = a.Z + b.Z
//     }

// let v1 = {
//     X = 3.0
//     Y = 2.0
//     Z = 2.0
// }

// let v2 = {
//     X = 2.0
//     Y = -3.0
//     Z = 1.0
// }

// let v3 = add v1 v2

//// You can annotate the type with a UoM and use it in function/method declaration
// type Vector<[<Measure>] 'Measure> = {
//     X : float
//     Y : float
//     Z : float
// }

// let add (a: Vector<'Measure>) (b: Vector<'Measure>) : Vector<'Measure> =
//     {
//         X = a.X + b.X
//         Y = a.Y + b.Y
//         Z = a.Z + b.Z
//     }

// let v1 : Vector<cm> = {
//     X = 3.0
//     Y = 2.0
//     Z = 2.0
// }

// let v2 : Vector<cm> = {
//     X = 2.0
//     Y = -3.0
//     Z = 1.0
// }

// let v3 = add v1 v2


// // You can define operators which ensure that the UoM match
// type Vector<[<Measure>] 'Measure> = {
//     X : float
//     Y : float
//     Z : float
// }
// with
//     static member (+) (a: Vector<'Measure>, b: Vector<'Measure>) : Vector<'Measure> =
//         {
//             X = a.X + b.X
//             Y = a.Y + b.Y
//             Z = a.Z + b.Z
//         }

// let v1 : Vector<cm> = {
//     X = 3.0
//     Y = 2.0
//     Z = 2.0
// }

// let v2 : Vector<cm> = {
//     X = 2.0
//     Y = -3.0
//     Z = 1.0
// }

// let v3 = v1 + v2

// // You can also make UoM work with other types

// type LengthX<[<Measure>] 'Measure> = {
//     X : float
// }

// type Vector<[<Measure>] 'Measure> = {
//     X : float
//     Y : float
//     Z : float
// }
// with
//     static member (+) (a: Vector<'Measure>, b: Vector<'Measure>) : Vector<'Measure> =
//         {
//             X = a.X + b.X
//             Y = a.Y + b.Y
//             Z = a.Z + b.Z
//         }

//     static member (+) (a: Vector<'Measure>, b: LengthX<'Measure>) : Vector<'Measure> =
//         {
//             X = a.X + b.X
//             Y = a.Y
//             Z = a.Z
//         }

// let v1 : Vector<cm> = {
//     X = 3.0
//     Y = 2.0
//     Z = 2.0
// }

// let lengthX : LengthX<cm> = { X = 10.0 }

// let v2 = v1 + lengthX



// Next steps
// There is an RFC which would ease working with UoM
// https://github.com/fsharp/fslang-suggestions/issues/892

// There's another for making UoM available to more types
// https://github.com/fsharp/fslang-suggestions/issues/563

// There is a library which extends which types UoM can be used with
// https://github.com/fsprojects/FSharp.UMX