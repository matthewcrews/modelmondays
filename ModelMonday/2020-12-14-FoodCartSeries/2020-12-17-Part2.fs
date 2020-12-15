namespace ModelMonday.FoodCart


open System
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure
open Spectre.Console


type Food = Food of string
type Increment = Increment of int
type DemandRate = DemandRate of float
[<Measure>] type USD
[<Measure>] type cm
[<Measure>] type gm
[<Measure>] type serving

type AnalysisResult = {
    BurgerQuantity : float<serving>
    PizzaQuantity : float<serving>
    TacoQuantity : float<serving>
    StorageUtilization : float
    FridgeUtilization : float
    WeightUtilization : float
    RevenueMean : float<USD>
    RevenueVariance : float<USD^2>
    RevenueStdDev : float<USD>
}

module Simulation =

    let sample 
        (foodDemands: seq<Food * DemandRate>)
        (revenue: SMap<Food, float<USD/serving>>)
        (plan: Map<Food, float<serving>>)
        (rng: System.Random) =
        
        let evaluteSoldQuantity planAmount (DemandRate demandRate) rng =
            // Generate a random sample from the Poisson distribution and take the lesser
            // of the planned inventory or of the random Demand value that was generated
            let actualQuantity = Math.Min (float planAmount, Sample.poisson demandRate rng |> float)
            // Multiply by 1.0<serving> to get the correct units on the result
            actualQuantity * 1.0<serving>

        foodDemands
        |> Seq.map (fun (food, demandRate) -> food, (evaluteSoldQuantity plan.[food] demandRate rng))
        |> Seq.sumBy (fun (food, soldQuantity) -> soldQuantity * revenue.[food])

    let evalute 
        (foodDemands: seq<Food * DemandRate>)
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


module PlanningModel =

    let createIncrementProbability
        (foodDemands: seq<Food * DemandRate>)
        (maxItems: int) =

        seq {
            for (food, DemandRate demandRate) in foodDemands do
                for i in 1..maxItems ->
                    let probability =  1.0 - (Poisson.CDF (demandRate - 1.0, (float i)))
                    (food, Increment i), probability
        } |> SMap2

    let create 
        (revenue: SMap<Food, float<USD/serving>>)
        (storage: SMap<Food, float<cm^3/serving>>)
        (fridgeSpace: SMap<Food, float<cm^3/serving>>)
        (weight: SMap<Food, float<gm/serving>>)
        (incrementProbability: SMap2<Food, Increment, float>)
        (packDecision: SMap2<Food, Increment, Decision<serving>>)
        (maxStorage: float<cm^3>)
        (maxWeight: float<gm>)
        (maxFridgeSpace: float<cm^3>) =

        let weightConstraint =
            Constraint.create "MaxWeight" (sum (weight .* packDecision) <== maxWeight)

        let storageConstraint =
            Constraint.create "MaxStorage" (sum (storage .* packDecision) <== maxStorage)

        let fridgeSpaceConstraint =
            Constraint.create "MaxFridgeSpace" (sum (fridgeSpace .* packDecision) <== maxFridgeSpace)

        let revenueExpectation =
            sum (revenue .* incrementProbability .* packDecision)

        let objective =
            Objective.create "MaxRevenueExpectation" Maximize revenueExpectation


        Model.create objective
        |> Model.addConstraint weightConstraint
        |> Model.addConstraint storageConstraint
        |> Model.addConstraint fridgeSpaceConstraint


module Example =

    let floor (x: float<'Measure>) =
        Math.Floor (float x)
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
            burger, DemandRate 600.0
            pizza,  DemandRate 900.0
            taco,   DemandRate 700.0
        ]

    let maxItems = 1_000
    let maxWeight = 1_000_000.0<gm>
    let maxStorage = 3_000_000.0<cm^3>
    let maxFridge = 2_000_000.0<cm^3>
    let numberOfSimulations = 1_000_000

    let simpleHeuristicRun () =

        let pizzaQuantity = 900.0<serving>

        let tacoQuantity =
            List.min [
                (maxStorage - (pizzaQuantity * storage.[pizza])) / storage.[taco]
                (maxFridge - (pizzaQuantity * fridgeSpace.[pizza])) / fridgeSpace.[taco]
                (maxWeight - (pizzaQuantity * weight.[pizza])) / weight.[taco]
            ] |> floor

        let burgerQuantity =
            List.min [
                (maxStorage - (pizzaQuantity * storage.[pizza]) - (tacoQuantity * storage.[taco])) / storage.[taco]
                (maxFridge - (pizzaQuantity * fridgeSpace.[pizza]) - (tacoQuantity * fridgeSpace.[taco])) / fridgeSpace.[taco]
                (maxWeight - (pizzaQuantity * weight.[pizza]) - (tacoQuantity * weight.[taco])) / weight.[taco]
            ] |> floor

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
        let stats = Simulation.evalute demandRates revenue plan rng numberOfSimulations

        {
            BurgerQuantity = burgerQuantity
            PizzaQuantity = pizzaQuantity
            TacoQuantity = tacoQuantity
            StorageUtilization = (storageUsage / maxStorage)
            FridgeUtilization = (fridgeUsage / maxFridge)
            WeightUtilization = (weightUsage / maxWeight)
            RevenueMean = stats.Mean * 1.0<USD>
            RevenueVariance = stats.Variance * 1.0<USD^2>
            RevenueStdDev = stats.StandardDeviation * 1.0<USD>
        }

    let optimizationRun () =

        let incrementProbabilities = PlanningModel.createIncrementProbability demandRates maxItems

        let packDecisions =
            DecisionBuilder<serving> "Pack" {
                for food in foods do
                    for increment in ([1..maxItems] |> List.map Increment) ->
                        Boolean
            } |> SMap2

        let planModel = PlanningModel.create revenue storage fridgeSpace weight incrementProbabilities packDecisions maxStorage maxWeight maxFridge

        let result = Solver.solve Settings.basic planModel

        match result with
        | Optimal solution ->
            let burgerQuantity = Solution.evaluate solution (sum packDecisions.[burger, All])
            let pizzaQuantity = Solution.evaluate solution (sum packDecisions.[pizza, All])
            let tacoQuantity = Solution.evaluate solution (sum packDecisions.[taco, All])

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

            let plan =
                [
                    burger, burgerQuantity
                    pizza, pizzaQuantity
                    taco, tacoQuantity
                ] |> Map

            let rng = System.Random ()
            let stats = Simulation.evalute demandRates revenue plan rng numberOfSimulations
            
            {
                BurgerQuantity = burgerQuantity
                PizzaQuantity = pizzaQuantity
                TacoQuantity = tacoQuantity
                StorageUtilization = (storageUsage / maxStorage)
                FridgeUtilization = (fridgeUsage / maxFridge)
                WeightUtilization = (weightUsage / maxWeight)
                RevenueMean = stats.Mean * 1.0<USD>
                RevenueVariance = stats.Variance * 1.0<USD^2>
                RevenueStdDev = stats.StandardDeviation * 1.0<USD>
            }

        | _ -> failwith "Failed to solve"

    let run () =

        let hueristicResult = simpleHeuristicRun ()
        let optimizationResult = optimizationRun ()

        let table = Table()
        table.AddColumn("Metric") |> ignore
        table.AddColumn("Heuristic") |> ignore
        table.AddColumn("Optimization") |> ignore

        table.AddRow("Burgers", $"{hueristicResult.BurgerQuantity}", $"{optimizationResult.BurgerQuantity}") |> ignore
        table.AddRow("Pizza", $"{hueristicResult.PizzaQuantity}", $"{optimizationResult.PizzaQuantity}") |> ignore
        table.AddRow("Taco", $"{hueristicResult.TacoQuantity}", $"{optimizationResult.TacoQuantity}") |> ignore
        
        table.AddRow("Storage Usage", $"%.2f{hueristicResult.StorageUtilization * 100.0}%%", $"%.2f{optimizationResult.StorageUtilization * 100.0}%%") |> ignore
        table.AddRow("Fridge Usage", $"%.2f{hueristicResult.FridgeUtilization * 100.0}%%", $"%.2f{optimizationResult.FridgeUtilization * 100.0}%%") |> ignore
        table.AddRow("Weight Usage", $"%.2f{hueristicResult.WeightUtilization * 100.0}%%", $"%.2f{optimizationResult.WeightUtilization * 100.0}%%") |> ignore

        table.AddRow("Revenue Mean", $"%.2f{hueristicResult.RevenueMean}", $"%.2f{optimizationResult.RevenueMean}") |> ignore
        table.AddRow("Revenue Variance", $"%.2f{hueristicResult.RevenueVariance}", $"%.2f{optimizationResult.RevenueVariance}") |> ignore
        table.AddRow("Revenue StdDev", $"%.2f{hueristicResult.RevenueStdDev}", $"%.2f{optimizationResult.RevenueStdDev}") |> ignore

        AnsiConsole.Render(table)
