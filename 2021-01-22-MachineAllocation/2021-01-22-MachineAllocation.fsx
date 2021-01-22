fsi.ShowDeclarationValues <- false

[<RequireQualifiedAccess>]
type JobType =
    | A
    | B
    | C

type Job = {
    Id : int
    JobType : JobType
    Length : float
} with
    override this.ToString () =
        $"Job_{this.Id}"

type Machine = {
    Id : int
    JobTypes : Set<JobType>
} with
    override this.ToString () =
        $"Machine_{this.Id}"


let jobTypes = 
    [|
        JobType.A
        JobType.B
        JobType.C
    |]

let jobTypeSets =
    [|
        Set jobTypes
        Set jobTypes.[1..]
        Set jobTypes.[..1]
    |]

let rng = System.Random(123)
let numberOfJobs = 100
let numberOfMachines = 10
let minJobLength = 1
let maxJobLength = 3
let maxWorkDifference = 5.0

let randomJobLength (rng: System.Random) =
    rng.Next(minJobLength, maxJobLength)
    |> float

let randomJobType (rng: System.Random) =
    jobTypes.[rng.Next(0, jobTypes.Length - 1)]

let randomJobTypeSet (rng: System.Random) =
    jobTypeSets.[rng.Next(0, jobTypeSets.Length - 1)]


let jobs =
    [1..numberOfJobs]
    |> List.map (fun id -> { 
        Id = id
        JobType = randomJobType rng
        Length = randomJobLength rng 
    })

let machines =
    [1..numberOfMachines]
    |> List.map (fun id -> {
        Id = id
        JobTypes = randomJobTypeSet rng
    })

module Map =

    let tryFindDefault (key: 'a) (d: 'v) (m: Map<'a, 'v>) =
        match Map.tryFind key m with
        | Some v -> v
        | None -> d


#r "nuget: Flips"

open Flips
open Flips.Types
open Flips.SliceMap

let jobMap =
    jobs
    |> List.groupBy (fun job -> job.JobType)
    |> Map

let jobLengths =
    jobs
    |> List.map (fun job -> job, job.Length)
    |> SMap

let assignments =
    DecisionBuilder "Assignment" {
        for machine in machines do
        for jobType in jobTypes do
        for job in Map.tryFindDefault jobType [] jobMap ->
            Boolean
    } |> SMap3

let minWork = Decision.createContinuous "MinWork" 0.0 infinity
let maxWork = Decision.createContinuous "MaxWork" 0.0 infinity

let maxWorkConstraints =
    ConstraintBuilder "MaxWork" {
        for machine in machines ->
            maxWork >== sum (assignments.[machine, All, All] .* jobLengths)
    }

let minWorkConstraints =
    ConstraintBuilder "MinWork" {
        for machine in machines ->
            minWork <== sum (assignments.[machine, All, All] .* jobLengths)
    }

let maxWorkDifferenceConstraint =
    Constraint.create "MaxWorkDifferent" (maxWork - minWork <== maxWorkDifference)

let setups =
    DecisionBuilder "Setups" {
        for machine in machines do
        for jobType in jobTypes ->
            Boolean
    } |> SMap2

let setupConstraints =
    ConstraintBuilder "SetupRequired" {
        for machine in machines do
        for jobType in jobTypes ->
            sum (assignments.[machine, jobType, All]) <== float numberOfJobs * setups.[machine, jobType]
    }

let numberSetupsExpression = sum setups
let minSetupsObjective = Objective.create "MinSetups" Minimize numberSetupsExpression

let jobsAssignmentConstraints =
    ConstraintBuilder "JobAssignment" {
        for job in jobs ->
            sum assignments.[All, All, job] == 1.0
    }

let model =
    Model.create minSetupsObjective
    |> Model.addConstraints maxWorkConstraints
    |> Model.addConstraints minWorkConstraints
    |> Model.addConstraint maxWorkDifferenceConstraint
    |> Model.addConstraints setupConstraints
    |> Model.addConstraints jobsAssignmentConstraints

let settings =
    { Settings.basic with MaxDuration = 60_000L * 10L }

let stopwatch = System.Diagnostics.Stopwatch()
stopwatch.Start()

let result = Solver.solve settings model
stopwatch.Stop()
printfn $"Elapsed ms: {stopwatch.ElapsedMilliseconds}"

match result with
| Optimal solution ->

    let assignmentValues =
        Solution.getValues solution assignments
        |> Map.filter (fun _ v -> v = 1.0)
        |> Map.toList
        |> List.map (fun ((machine, _, job), _) -> machine, job)
        |> List.sortBy (fun (machine, job) -> machine.Id, job.Id)

    printfn "Assignments:"
    for (machine, job) in assignmentValues do
        printfn $"Machine: {machine.Id} | Job: {job.Id}"


    let machineLoads =
        assignmentValues
        |> List.groupBy fst
        |> List.map (fun (machine, grp) -> machine, grp |> List.map snd |> List.sumBy (fun x -> x.Length))

    printfn ""
    printfn "Machine Loads:"
    for (machine, load) in machineLoads do
        printfn $"Machine: {machine.Id} | Load: {load}"


    let maxDifference =
        let loads = machineLoads |> List.map snd
        (List.max loads) - (List.min loads)

    printfn ""
    printfn $"Max Diffence In Loading: { maxDifference }"

| _ -> printfn "%A" result

