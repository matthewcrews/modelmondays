namespace ModelMonday.FoodCart.Part1


open System
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure
open Spectre.Console


type Food = Food of string
type Lambda = Lambda of float
[<Measure>] type USD
[<Measure>] type cm
[<Measure>] type gm
[<Measure>] type serving


module Simulation =

    let sample 
        (foodDemands: seq<Food * Lambda>)
        (revenue: SMap<Food, float<USD/serving>>)
        (plan: Map<Food, float<serving>>)
        (rng: System.Random) =
        
        let evaluteSoldQuantity planAmount (Lambda lambda) rng =
            // Generate a random sample from the Poisson distribution and take the lesser
            // of the planned inventory or of the random Demand value that was generated
            let actualQuantity = Math.Min (float planAmount, Sample.poisson lambda rng |> float)
            // Multiply by 1.0<serving> to get the correct units on the result
            actualQuantity * 1.0<serving>

        foodDemands
        |> Seq.map (fun (food, demandRate) -> food, (evaluteSoldQuantity plan.[food] demandRate rng))
        |> Seq.sumBy (fun (food, soldQuantity) -> soldQuantity * revenue.[food])

    let evalute 
        (foodDemands: seq<Food * Lambda>)
        (revenue: SMap<Food, float<USD/serving>>)
        (plan: Map<Food, float<serving>>)
        (rng: System.Random)
        (numberSamples: int) =

        let samples =
            seq {
                for _ in 1..numberSamples ->
                    sample foodDemands revenue plan rng
                    |> float
            } |> Array.ofSeq

        DescriptiveStatistics samples





module Example =

    // A function to call Math.Floor on floats with Units of Measure
    let floor (x: float<'Measure>) =
        Math.Floor (float x)
        |> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure>

    // A function to call Math.Min on floats with Units of Measure
    let min (a: float<'Measure>, b: float<'Measure>) =
        Math.Min (float a, float b)
        |> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure>

    let burger = Food "Burger"
    let pizza = Food "Pizza"
    let taco = Food "Taco"

    let foods =
        [
            burger
            pizza
            taco
        ]

    let revenue = 
        [
            burger, 1.3<USD/serving>
            pizza,  1.6<USD/serving>
            taco,   1.4<USD/serving>
        ] |> SMap

    let storage =
        [
            burger, 700.0<cm^3/serving>
            pizza,  950.0<cm^3/serving>
            taco,   800.0<cm^3/serving>
        ] |> SMap

    let fridgeSpace =
        [
            burger, 900.0<cm^3/serving>
            pizza,  940.0<cm^3/serving>
            taco,   850.0<cm^3/serving>
        ] |> SMap

    let weight =
        [
            burger, 550.0<gm/serving>
            pizza,  800.0<gm/serving>
            taco,   600.0<gm/serving>
        ] |> SMap

    let demandRates =
        [
            burger, Lambda 600.0
            pizza,  Lambda 900.0
            taco,   Lambda 700.0
        ]

    let maxItems = 1_000
    let maxWeight = 1_000_000.0<gm>
    let maxStorage = 3_000_000.0<cm^3>
    let maxFridge = 2_000_000.0<cm^3>

    let run () =

        // We packe the average daily demand of pizzas
        let pizzaQuantity = 900.0<serving>

        let tacoQuantity =
            // The number of possible tacos based on space
            let tacosBasedOnSpace =
                List.min [
                    (maxStorage - (pizzaQuantity * storage.[pizza])) / storage.[taco]
                    (maxFridge - (pizzaQuantity * fridgeSpace.[pizza])) / fridgeSpace.[taco]
                    (maxWeight - (pizzaQuantity * weight.[pizza])) / weight.[taco]
                ] |> floor
            // The min of taco demand and the space available
            min (700.0<serving>, tacosBasedOnSpace)

        let burgerQuantity =
            // The number of possible burgers based on space
            let burgersBasedOnSpace =
                List.min [
                    (maxStorage - (pizzaQuantity * storage.[pizza]) - (tacoQuantity * storage.[taco])) / storage.[taco]
                    (maxFridge - (pizzaQuantity * fridgeSpace.[pizza]) - (tacoQuantity * fridgeSpace.[taco])) / fridgeSpace.[taco]
                    (maxWeight - (pizzaQuantity * weight.[pizza]) - (tacoQuantity * weight.[taco])) / weight.[taco]
                ] |> floor
            // The min of burgers demand and the space available
            min (600.0<serving>, burgersBasedOnSpace)

        let plan =
            [
                burger, burgerQuantity
                pizza, pizzaQuantity
                taco, tacoQuantity
            ] |> Map

        let storageUsage = 
            burgerQuantity * storage.[burger] + 
            pizzaQuantity * storage.[pizza] + 
            tacoQuantity * storage.[taco]
        
        let fridgeUsage =
            burgerQuantity * fridgeSpace.[burger] + 
            pizzaQuantity * fridgeSpace.[pizza] + 
            tacoQuantity * fridgeSpace.[taco]
        
        let weightUsage = 
            burgerQuantity * weight.[burger] + 
            pizzaQuantity * weight.[pizza] + 
            tacoQuantity * weight.[taco]

        let rng = System.Random ()
        let stats_100Runs = Simulation.evalute demandRates revenue plan rng 100
        let stats_1_000Runs = Simulation.evalute demandRates revenue plan rng 1_000
        let stats_10_000Runs = Simulation.evalute demandRates revenue plan rng 10_000
        let stats_100_000Runs = Simulation.evalute demandRates revenue plan rng 100_000
        let stats_1_000_000Runs = Simulation.evalute demandRates revenue plan rng 1_000_000

        let table = Table()
        table.AddColumn("NumberOfRuns") |> ignore
        table.AddColumn("Mean") |> ignore
        table.AddColumn("Variance") |> ignore
        table.AddColumn("StdDev") |> ignore
        table.AddColumn("95% CI") |> ignore
        table.AddColumn("99% CI") |> ignore

        let createRowString (stats: DescriptiveStatistics) =
            let ci95 = $"%.2f{stats.Mean - 1.96 * (stats.StandardDeviation / (Math.Sqrt (float stats.Count)))}, %.2f{stats.Mean + 1.96 * (stats.StandardDeviation / (Math.Sqrt (float stats.Count)))}"
            let ci99 = $"%.2f{stats.Mean - 2.567 * (stats.StandardDeviation / (Math.Sqrt (float stats.Count)))}, %.2f{stats.Mean + 2.567 * (stats.StandardDeviation / (Math.Sqrt (float stats.Count)))}"
            [|
                String.Format("{0:#,###}", stats.Count)
                $"%.2f{stats.Mean}"
                $"%.2f{stats.Variance}"
                $"%.2f{stats.StandardDeviation}"
                ci95
                ci99
            |]

        table.AddRow(createRowString stats_100Runs) |> ignore
        table.AddRow(createRowString stats_1_000Runs) |> ignore
        table.AddRow(createRowString stats_10_000Runs) |> ignore
        table.AddRow(createRowString stats_100_000Runs) |> ignore
        table.AddRow(createRowString stats_1_000_000Runs) |> ignore

        AnsiConsole.Render(table)
