// Learn more about F# at http://fsharp.org

open System
open ModelMonday

[<EntryPoint>]
let main argv =
    printfn "Welcome to Model Mondays!"

    //SecretSantaExchange.example ()
    //ModelMonday.FoodCart.Part1.Example.run ()
    ModelMonday.FoodCart.Part2.Example.run ()
    //FoodCart.Example.run ()

    printfn "Press any key to exit..."
    Console.ReadKey() |> ignore 
    0 // return an integer exit code
