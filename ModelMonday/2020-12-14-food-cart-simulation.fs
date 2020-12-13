namespace ModelMonday.FoodCart


open System
open MathNet.Numerics.Distributions
open MathNet.Numerics.Statistics
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

type Food = Food of string
type Increment = Increment of int
type DemandRate = DemandRate of float
[<Measure>] type USD
[<Measure>] type cm
[<Measure>] type gm
[<Measure>] type item


module Simulation =

    //type Parameters = {
    //    BurgerDemand : float
    //    PizzaDemand : float
    //    TacoDemand : float
    //    BurgerRevenue : float<USD>
    //    PizzaRevenue : float<USD>
    //    TacoRevenue : float<USD>
    //}

    //type Plan = {
    //    BurgerQuantity : float
    //    PizzaQuantity : float
    //    TacoQuantity : float
    //}

    type Evaluation = {
        RevenueMean : float<USD>
        RevenueVariance : float<USD^2>
        RevenueStdDev : float<USD>
    }

    let sample 
        (foodDemands: seq<Food * DemandRate>)
        (revenue: SMap<Food, float<USD/item>>)
        (plan: Map<Food, float<item>>)
        (rng: System.Random) =
        
        let evaluteSoldQuantity planAmount (DemandRate demandRate) rng =
            let actualQuantity = Math.Min (float planAmount, Sample.poisson demandRate rng |> float)
            actualQuantity * 1.0<item>

        foodDemands
        |> Seq.map (fun (food, demandRate) -> food, (evaluteSoldQuantity plan.[food] demandRate rng))
        |> Seq.sumBy (fun (food, soldQuantity) -> soldQuantity * revenue.[food])


    module Plan =

        let evalute 
            (foodDemands: seq<Food * DemandRate>)
            (revenue: SMap<Food, float<USD/item>>)
            (plan: Map<Food, float<item>>)
            (rng: System.Random)
            (numberSamples: int) =

            let samples =
                seq {
                    for _ in 1..numberSamples ->
                        sample foodDemands revenue plan rng
                        |> float
                } |> Array.ofSeq

            let stats = DescriptiveStatistics samples

            {
                RevenueMean = stats.Mean * 1.0<USD>
                RevenueVariance = stats.Variance * 1.0<USD^2>
                RevenueStdDev = stats.StandardDeviation * 1.0<USD>
            }


module PlanningModel =

    let createIncrementProbability
        (foodDemands: seq<Food * DemandRate>)
        (maxItems: int) =

        seq {
            for (food, DemandRate demandRate) in foodDemands do
                for i in 1..maxItems ->
                    let probability =  1.0 - (Poisson.CDF (demandRate, (float i)))
                    (food, Increment i), probability
        } |> SMap2

    let create 
        (revenue: SMap<Food, float<USD/item>>)
        (storage: SMap<Food, float<cm^3/item>>)
        (fridgeSpace: SMap<Food, float<cm^3/item>>)
        (weight: SMap<Food, float<gm/item>>)
        (incrementProbability: SMap2<Food, Increment, float>)
        (packDecision: SMap2<Food, Increment, Decision<item>>)
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
            burger, 1.0<USD/item>
            pizza,  2.0<USD/item>
            taco,   1.5<USD/item>
        ] |> SMap

    let storage =
        [
            burger, 30.0<cm^3/item>
            pizza,  45.0<cm^3/item>
            taco,   20.5<cm^3/item>
        ] |> SMap

    let fridgeSpace =
        [
            burger, 25.0<cm^3/item>
            pizza,  30.0<cm^3/item>
            taco,   15.0<cm^3/item>
        ] |> SMap

    let weight =
        [
            burger, 300.0<gm/item>
            pizza,  350.0<gm/item>
            taco,   280.0<gm/item>
        ] |> SMap

    let demandRates =
        [
            burger, DemandRate 50.0
            pizza,  DemandRate 60.0
            taco,   DemandRate 45.0
        ]

    let run () =

        let maxItems = 100
        let maxWeight = 1_000_000.0<gm>
        let maxStorage = 3_000_000.0<cm^3>
        let maxFridge = 2_000_000.0<cm^3>

        let incrementProbabilities = PlanningModel.createIncrementProbability demandRates maxItems

        let packDecisions =
            DecisionBuilder<item> "Pack" {
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
            let plan =
                [
                    burger, burgerQuantity
                    pizza, pizzaQuantity
                    taco, tacoQuantity
                ] |> Map
            let rng = System.Random ()
            let evaluation = Simulation.Plan.evalute demandRates revenue plan rng 10_000
            printfn "%A" evaluation

        | _ -> printfn "Failed to solve"


            //let rng = System.Random ()

            //let plan =
            //    [
            //        burger, 10.0<item>
            //        pizza, 10.0<item>
            //        taco, 10.0<item>
            //    ] |> Map

            //let x = Simulation.Plan.evalute demandRates revenue plan rng 10_000

            //printfn "%A" x


