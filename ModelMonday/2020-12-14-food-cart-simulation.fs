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

    type PoissonParameter = {
        Lambda : float
    }

    type Parameters = {
        BurgerDemand : float
        PizzaDemand : float
        TacoDemand : float
        BurgerRevenue : float<USD>
        PizzaRevenue : float<USD>
        TacoRevenue : float<USD>
    }

    type Plan = {
        BurgerQuantity : float
        PizzaQuantity : float
        TacoQuantity : float
    }

    type Evaluation = {
        RevenueMean : float<USD>
        RevenueVariance : float<USD^2>
        RevenueStdDev : float<USD>
    }

    let sample (parameters: Parameters) (plan: Plan) (rng: System.Random) =

        let tacoRevenue = 
            let tacoDemand = 
                Sample.poisson parameters.TacoDemand rng 
                |> float
            let tacoSold = Math.Max (tacoDemand, plan.TacoQuantity)
            tacoSold * parameters.TacoRevenue

        let burgerRevenue = 
            let burgerDemand = 
                Sample.poisson parameters.BurgerDemand rng 
                |> float
            let burgerSold = Math.Max (burgerDemand, plan.BurgerQuantity)
            burgerSold * parameters.BurgerRevenue

        let pizzaRevenue = 
            let pizzaDemand = 
                Sample.poisson parameters.PizzaDemand rng 
                |> float
            let pizzaSold = Math.Max (pizzaDemand, plan.PizzaQuantity)
            pizzaSold * parameters.PizzaRevenue

        tacoRevenue + burgerRevenue + pizzaRevenue

    module Plan =

        let evalute (parameters: Parameters) (plan: Plan) (rng: System.Random) (numberSamples: int) =

            let samples =
                seq {
                    for _ in 1..numberSamples ->
                        sample parameters plan rng
                        |> float
                } |> Array.ofSeq

            let stats = DescriptiveStatistics samples

            {
                RevenueMean = stats.Mean * 1.0<USD>
                RevenueVariance = stats.Variance * 1.0<USD^2>
                RevenueStdDev = stats.StandardDeviation * 1.0<USD>
            }


module PlanningModel =

    let createIncrementProbability (foodDemands: seq<Food * DemandRate>) (maxItems: int) : SMap2<Food, Increment, float> =

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

    open Simulation

    let foods =
        [
            Food "Burger"
            Food "Pizza" 
            Food "Taco"  
        ]

    let revenue = 
        [
            Food "Burger", 1.0<USD/item>
            Food "Pizza",  2.0<USD/item>
            Food "Taco",   1.5<USD/item>
        ] |> SMap

    let storage =
        [
            Food "Burger", 30.0<cm^3/item>
            Food "Pizza",  45.0<cm^3/item>
            Food "Taco",   20.5<cm^3/item>
        ] |> SMap

    let fridgeSpace =
        [
            Food "Burger", 25.0<cm^3/item>
            Food "Pizza",  30.0<cm^3/item>
            Food "Taco",   15.0<cm^3/item>
        ] |> SMap

    let weight =
        [
            Food "Burger", 300.0<gm/item>
            Food "Pizza",  350.0<gm/item>
            Food "Taco",   280.0<gm/item>
        ] |> SMap

    let demandRates =
        [
            Food "Burger", DemandRate 50.0
            Food "Pizza",  DemandRate 60.0
            Food "Taco",   DemandRate 45.0
        ]

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

    //let result = Solver.solve Settings.basic planModel

    //match result with
    //| Optimal solution ->
    //    let burgerQuantity = Solution.evaluate solution (sum packDecisions.[Food "Burger", All])
    //    let pizzaQuantity = Solution.evaluate solution (sum packDecisions.[Food "Pizza", All])
    //    let tacoQuantity = Solution.evaluate solution (sum packDecisions.[Food "Taco", All])



    //| _ -> printfn "Failed to solve"
    let rng = System.Random ()
    let parameters = {
        BurgerDemand = 10.0
        PizzaDemand = 10.0
        TacoDemand = 10.0
        BurgerRevenue = 1.0<USD>
        PizzaRevenue = 1.0<USD>
        TacoRevenue = 1.0<USD>
    }

    let plan = {
        BurgerQuantity = 10.0
        PizzaQuantity = 10.0
        TacoQuantity = 10.0
    }


    let x = Simulation.Plan.evalute parameters plan rng 100

    printfn "%A" x


