﻿// Learn more about F# at http://fsharp.org

open System
open ModelMonday

[<EntryPoint>]
let main argv =
    printfn "Welcome to Model Mondays!"

    SecretSantaExchange.example ()


    printfn "Press any key to exit..."
    Console.ReadKey() |> ignore 
    0 // return an integer exit code