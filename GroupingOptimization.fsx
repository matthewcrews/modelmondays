#r "nuget: Flips"

[<Measure>] type kg

type Grade =
    | Premium
    | Good
    | Fair

type Berry =
    | Strawberry
    | Blueberry
    | Blackberry
    | Marionberry

type Basket = {
    Berry : Berry
    Grade : Grade
    Weight : float<kg>
}

type 