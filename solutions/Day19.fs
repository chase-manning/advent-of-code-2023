module Day19

open System

type Category =
    | X
    | M
    | A
    | S


type Step =
    { autoReturn: bool
      category: Category
      greaterThan: bool
      value: int
      output: string }

type Workflow = { name: string; steps: Step List }

type Part = { x: int; m: int; a: int; s: int }

let getStep (input: string) : Step =
    let parts = input.Split(":")

    if parts.Length = 1 then
        { autoReturn = true
          category = X
          greaterThan = false
          value = 0
          output = parts.[0] }
    else
        let categoryChar = parts.[0].[0]
        let greaterThan = parts.[0].Substring(1, 1) = ">"
        let valueString = parts.[0].Substring(2, parts.[0].Length - 2)
        let value = int valueString

        { autoReturn = false
          category =
            match categoryChar with
            | 'x' -> X
            | 'm' -> M
            | 'a' -> A
            | _ -> S
          greaterThan = greaterThan
          value = value
          output = parts.[1] }


let getWorkflow (input: string) : Workflow =
    let startIndex = input.IndexOf("{")
    let name = input.Substring(0, startIndex).Trim()

    let stepInputs =
        input.Substring(startIndex + 1, input.Length - startIndex - 2).Split(",")

    let steps = stepInputs |> Array.toList |> List.map getStep
    { name = name; steps = steps }

let getWorkflows (input: string List) : Map<string, Workflow> =
    let rec run (workflows: Map<string, Workflow>) (index: int) =
        let line = input.[index]

        if line = "" then
            workflows
        else
            let workflow = getWorkflow line
            let newWorkflows = workflows.Add(workflow.name, workflow)
            run newWorkflows (index + 1)

    run Map.empty 0

let getParts (input: string List) : Part List =
    let rec run (parts: Part List) (index) : Part List =
        let line = input.[index]

        if line = "" then
            parts
        else
            let trimmed = line.Substring(1, line.Length - 2)
            let values = trimmed.Split(",")
            let x = int (values.[0].Split("=").[1])
            let m = int (values.[1].Split("=").[1])
            let a = int (values.[2].Split("=").[1])
            let s = int (values.[3].Split("=").[1])
            let newPart = { x = x; m = m; a = a; s = s }
            run (newPart :: parts) (index - 1)

    run [] (input.Length - 1)

let isAccepted (part: Part) (workflows: Map<string, Workflow>) : bool =
    let rec run (workflow: string) : bool =
        let workflow = workflows.[workflow]

        let rec runSteps (index: int) : string =
            let step = workflow.steps.[index]

            if step.autoReturn then
                step.output
            else
                let value =
                    match step.category with
                    | X -> part.x
                    | M -> part.m
                    | A -> part.a
                    | S -> part.s

                if step.greaterThan then
                    if value > step.value then
                        step.output
                    else
                        runSteps (index + 1)
                else if value < step.value then
                    step.output
                else
                    runSteps (index + 1)

        let workFlow = runSteps 0

        if workFlow = "A" then true
        else if workFlow = "R" then false
        else run workFlow

    run "in"

let partTotal (part: Part) : int = part.x + part.m + part.a + part.s

let part1 (input: string List) =
    let workflows = getWorkflows input
    printfn "%A" workflows

    let parts = getParts input
    printfn "%A" parts

    let accepted = parts |> List.filter (fun part -> isAccepted part workflows)

    printfn "%A" accepted

    let total = accepted |> List.map partTotal |> List.sum
    total |> string

let part2 (input: string List) = "meow"
