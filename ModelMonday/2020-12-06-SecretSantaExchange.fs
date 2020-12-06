namespace ModelMonday

open Flips
open Flips.Types
open Flips.SliceMap
open Spectre.Console

module SecretSantaExchange =

    type Reindeer = Reindeer of string
    type Giver = Giver of Reindeer
    type Receiver = Receiver of Reindeer
    type SecretSanta = {
        Reindeer : Reindeer
        PreviousReceivers : Receiver list
    }

    let findAssignments (santas:SecretSanta list) =

        let reindeer =
            santas
            |> List.map (fun s -> s.Reindeer)
            |> Set

        let givers = reindeer |> Set.map Giver
        let receivers = reindeer |> Set.map Receiver

        let penalty =
            [ for s in santas do
                // Get the number of receivers once
                let numberOfReceivers = s.PreviousReceivers.Length
                s.PreviousReceivers
                |> List.mapi (fun idx r -> ((Giver s.Reindeer), r), float (numberOfReceivers - idx))
            ] |> List.concat
            |> SMap2
        
        let possiblePairings =
            seq { for giver in reindeer do
                    // We only want pairings with different reindeer
                    for receiver in reindeer.Remove giver ->
                        (Giver giver, Receiver receiver)
            }

        let assignDecisions =
            DecisionBuilder "Assignment" {
                for pairing in possiblePairings ->
                    Boolean
            } |> SMap2

        let giveOnlyOnce =
            ConstraintBuilder "GiveOnlyOnce" {
                for giver in givers ->
                    sum assignDecisions.[giver, All] == 1.0
            }

        let receiveOnlyOnce =
            ConstraintBuilder "ReceiveOnlyOnce" {
                for receiver in receivers ->
                    sum assignDecisions.[All, receiver] == 1.0
            }

        let penaltyExpression = sum (penalty .* assignDecisions)
        let objective = 
            Objective.create "MinimizePreviousPairings" Minimize penaltyExpression

        let model =
            Model.create objective
            |> Model.addConstraints giveOnlyOnce
            |> Model.addConstraints receiveOnlyOnce

        let result = Solver.solve Settings.basic model

        match result with
        | Optimal solution ->
            let selectedPairings =
                Solution.getValues solution assignDecisions
                |> Map.filter (fun pair value -> value = 1.0)
                |> Map.toSeq
                |> Seq.map fst
            Result.Ok selectedPairings
        | _ -> Result.Error "Unable to find pairings"

    
    let prettyPrintResults (pairings: seq<Giver * Receiver>) =
        let table = Table()
        table.AddColumn("Giver") |> ignore
        table.AddColumn("Receiver") |> ignore

        for (Giver (Reindeer g), Receiver (Reindeer r)) in pairings do
            table.AddRow(g, r) |> ignore

        AnsiConsole.Render(table)


    let example () =

        let santas =
            [
                { Reindeer = Reindeer "Rudolph"; PreviousReceivers = [ Receiver (Reindeer "Blitzen")]}
                { Reindeer = Reindeer "Dasher";  PreviousReceivers = [ Receiver (Reindeer "Vixen")]}
                { Reindeer = Reindeer "Dancer";  PreviousReceivers = [ Receiver (Reindeer "Rudolph")]}
                { Reindeer = Reindeer "Prancer"; PreviousReceivers = [ Receiver (Reindeer "Cupid")]}
                { Reindeer = Reindeer "Vixen";   PreviousReceivers = [ Receiver (Reindeer "Dancer")]}
                { Reindeer = Reindeer "Comet";   PreviousReceivers = [ Receiver (Reindeer "Dasher")]}
                { Reindeer = Reindeer "Cupid";   PreviousReceivers = [ Receiver (Reindeer "Donner")]}
                { Reindeer = Reindeer "Donner";  PreviousReceivers = [ Receiver (Reindeer "Comet")]}
                { Reindeer = Reindeer "Blitzen"; PreviousReceivers = [ Receiver (Reindeer "Prancer")]}
            ]

        let findResult = findAssignments santas

        match findResult with
        | Ok pairings -> prettyPrintResults pairings
        | Error _ -> printfn "No Christmas this year 😞"

