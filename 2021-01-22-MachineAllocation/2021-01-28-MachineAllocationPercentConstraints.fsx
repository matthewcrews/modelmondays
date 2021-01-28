open System.Collections.Generic
fsi.ShowDeclarationValues <- false
#r "nuget: Flips"

open Flips
open Flips.Types
open Flips.SliceMap


module Types =

    // The Domain
    [<RequireQualifiedAccess>]
    type JobType =
        | A
        | B
        | C
        | D

    type Job = {
        Id : int
        JobType : JobType
        Size : float
    } with
        override this.ToString () =
            $"Job_{this.Id}"

    type Machine = {
        Id : int
        JobTypes : Set<JobType>
    } with
        override this.ToString () =
            $"Machine_{this.Id}"


module DataGeneration =

    open System
    open Types

    // Set of JobTypes for iterating over and sampling from
    let jobTypes = 
        [|
            JobType.A
            JobType.B
            JobType.C
            JobType.D
        |]

    // Some theoretical JobTypeSets to be used in generating
    // random Machines
    let jobTypeSets =
        [|
            Set jobTypes
            Set jobTypes.[1..]
            Set jobTypes.[2..]
        |]

    let minJobSize = 1
    let maxJobSize = 3

    let randomJobSize (rng: Random) =
        rng.Next(minJobSize, maxJobSize)
        |> float

    let randomJobType (rng: Random) =
        jobTypes.[rng.Next(0, jobTypes.Length)]

    let randomJobTypeSet (rng: Random) =
        jobTypeSets.[rng.Next(0, jobTypeSets.Length)]

    let randomJob (rng: Random) (id: int) =
        { 
            Id = id
            JobType = randomJobType rng
            Size = randomJobSize rng 
        }

    let randomMachine (rng: Random) (id: int) =
        {
            Id = id
            JobTypes = randomJobTypeSet rng
        }
    

open Types

// Setting up parameters for the example
let rng = System.Random(123)
let numberOfJobs = 100
let numberOfMachines = 5
let maxWorkDifference = 2.0

// Create some examples jobs
let jobs =
    [1..numberOfJobs]
    |> List.map (DataGeneration.randomJob rng)

// Create some test machines
let machines =
    [1..numberOfMachines]
    |> List.map (DataGeneration.randomMachine rng)


// A Map from JobType to the Jobs which are of that type
let jobsForJobType =
    jobs
    |> List.groupBy (fun job -> job.JobType)
    |> Map

// A SliceMap where the key is a Job and the value is the length of the Job
let jobSizes =
    jobs
    |> List.map (fun job -> job, job.Size)
    |> SMap



// The Decisions which represent assigning a Job to a Machine
// The JobType index allows us to slice along the job type
// which is useful in some of the constraints
let assignments =
    DecisionBuilder "Assignment" {
        for machine in machines do
        for jobType in machine.JobTypes do
        for job in jobsForJobType |> Map.tryFind jobType |> Option.defaultValue [] ->
            Boolean
    } |> SMap3

// Each job must be assigned
let jobsAssignmentConstraints =
    ConstraintBuilder "JobAssignment" {
        for job in jobs ->
            sum assignments.[All, All, job] == 1.0
    }

// A Decision which is meant to represent the MaxWork value across all Machines
let maxWork = Decision.createContinuous "MaxWork" 0.0 infinity
// A Decision which is meant to represent the MinWork value across all Machines
let minWork = Decision.createContinuous "MinWork" 0.0 infinity

// We constrain the difference between the most heavily loaded machine
// and the least loaded
let maxWorkDifferenceConstraint =
    Constraint.create "MaxWorkDifferent" (maxWork - minWork <== maxWorkDifference)


// The maxWork Decision must be greater or equal to all of the total work
// for each Machine
let maxWorkConstraints =
    ConstraintBuilder "MaxWork" {
        for machine in machines ->
            maxWork >== sum (assignments.[machine, All, All] .* jobSizes)
    }

// The minWork Decision must be less or equal to all of the total work
// for each Machine
let minWorkConstraints =
    ConstraintBuilder "MinWork" {
        for machine in machines ->
            minWork <== sum (assignments.[machine, All, All] .* jobSizes)
    }

// A Decision which indicates whether we setup a given Machine for a 
// JobType at any point
let setups =
    DecisionBuilder "Setups" {
        for machine in machines do
        for jobType in machine.JobTypes ->
            Boolean
    } |> SMap2

// We must turn the setups value for a given Machine and JobType to 1
// if we assign a Job of the given JobType to the Machine
let setupConstraints =
    ConstraintBuilder "SetupRequired" {
        for machine in machines do
        for jobType in machine.JobTypes ->
            sum (assignments.[machine, jobType, All]) <== (float numberOfJobs) * setups.[machine, jobType]
    }

// Limit on the amount of JobType D on any given machine
let maxJobTypeDPercentage = 0.50

let maxJobTypeDConstraints =
    ConstraintBuilder "MaxTypeD" {
        for machine in machines ->
            sum (assignments.[machine, JobType.D, All] .* jobSizes) <== maxJobTypeDPercentage * sum (assignments.[machine, All, All] .* jobSizes)
    }


// An expression which is the sum of the Setups that will need to be performed
let numberSetupsExpression = sum setups
// We want to minimize the number of setups
let minSetupsObjective = Objective.create "MinSetups" Minimize numberSetupsExpression

// Compose the model
let model =
    Model.create minSetupsObjective
    |> Model.addConstraints maxWorkConstraints
    |> Model.addConstraints minWorkConstraints
    |> Model.addConstraint maxWorkDifferenceConstraint
    |> Model.addConstraints setupConstraints
    |> Model.addConstraints jobsAssignmentConstraints
    |> Model.addConstraints maxJobTypeDConstraints

// Give the solver plenty of time to find a solution
let settings = { Settings.basic with MaxDuration = 60_000L }

let result = Solver.solve settings model

let getMachineAssignments (solution: Solution) (assignments: SMap3<Machine, JobType, Job, Decision>) =
    Solution.getValues solution assignments
    |> Map.filter (fun _ v -> v = 1.0)
    |> Map.toList
    |> List.map (fun ((machine, _, job), _) -> machine, job)
    |> List.sortBy (fun (machine, job) -> machine.Id, job.Id)
    |> List.groupBy fst
    |> List.map (fun (machine, jobs) -> machine, jobs |> List.map snd)

let getMachineLoading jobAssignments =
    jobAssignments
    |> List.map (fun (machine, jobs) -> 
        {| Machine = machine
           TotalWork = 
                jobs 
                |> List.sumBy (fun j -> j.Size)
           JobTypeDWork = 
                jobs 
                |> List.filter (fun j -> j.JobType = JobType.D) 
                |> List.sumBy (fun j -> j.Size)
        |})

match result with
| Optimal solution ->

    // Get which jobs are assigned to each machine
    let machineAssignments = getMachineAssignments solution assignments

    // Calculate the total work for each machine and the amount of job-type D
    let machineLoads = getMachineLoading machineAssignments

    printfn ""
    printfn "Machine Loading:"
    for (m) in machineLoads do
        // Print out the loading for each machine and the percent of job-type D work
        printfn $"Machine: {m.Machine.Id} | Total Work: {m.TotalWork} | Type D Work %.2f{m.JobTypeDWork / m.TotalWork} %%"

    // Find the min and max loads and calculate the difference
    let maxDifference =
        let loads = machineLoads |> List.map (fun m -> m.TotalWork)
        (List.max loads) - (List.min loads)

    printfn ""
    printfn $"Max Difference in Loading: { maxDifference }"

| _ -> printfn "%A" result

