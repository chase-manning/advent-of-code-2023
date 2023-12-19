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

type PropertyRange = { min: int; max: int }

type PartRange =
    { value: string
      x: PropertyRange
      m: PropertyRange
      a: PropertyRange
      s: PropertyRange }

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

let getPartRanges (workflows: Map<string, Workflow>) : PartRange List =
    let rec run (partRange: PartRange) : PartRange List =
        if partRange.value = "A" || partRange.value = "R" then
            [ partRange ]
        else
            let workflow = workflows.[partRange.value]

            let rec runSteps (index: int) (part: PartRange) : PartRange List =
                let step = workflow.steps.[index]

                if step.autoReturn then
                    run
                        { value = step.output
                          x = part.x
                          m = part.m
                          a = part.a
                          s = part.s }
                else if step.greaterThan then
                    match step.category with
                    | X ->
                        let matchingPartRangeInput: PartRange =
                            { value = step.output
                              x =
                                { min = step.value + 1
                                  max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = step.value }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRange = runSteps (index + 1) notMatchingPartRangeInput
                        let matchingPartRange = run matchingPartRangeInput
                        matchingPartRange @ notMatchingPartRange
                    | M ->
                        let matchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m =
                                { min = step.value + 1
                                  max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = step.value }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRange = runSteps (index + 1) notMatchingPartRangeInput
                        let matchingPartRange = run matchingPartRangeInput
                        matchingPartRange @ notMatchingPartRange
                    | A ->
                        let matchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a =
                                { min = step.value + 1
                                  max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = step.value }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRange = runSteps (index + 1) notMatchingPartRangeInput
                        let matchingPartRange = run matchingPartRangeInput
                        matchingPartRange @ notMatchingPartRange
                    | S ->
                        let matchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s =
                                { min = step.value + 1
                                  max = part.s.max } }

                        let notMatchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = step.value } }

                        let notMatchingPartRange = runSteps (index + 1) notMatchingPartRangeInput
                        let matchingPartRange = run matchingPartRangeInput
                        matchingPartRange @ notMatchingPartRange
                else
                    match step.category with
                    | X ->
                        let matchingPartRangeInput: PartRange =
                            { value = step.output
                              x =
                                { min = part.x.min
                                  max = step.value - 1 }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = step.value; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRange = runSteps (index + 1) notMatchingPartRangeInput
                        let matchingPartRange = run matchingPartRangeInput
                        matchingPartRange @ notMatchingPartRange
                    | M ->
                        let matchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m =
                                { min = part.m.min
                                  max = step.value - 1 }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = step.value; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRange = runSteps (index + 1) notMatchingPartRangeInput
                        let matchingPartRange = run matchingPartRangeInput
                        matchingPartRange @ notMatchingPartRange
                    | A ->
                        let matchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a =
                                { min = part.a.min
                                  max = step.value - 1 }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = step.value; max = part.a.max }
                              s = { min = part.s.min; max = part.s.max } }

                        let notMatchingPartRange = runSteps (index + 1) notMatchingPartRangeInput
                        let matchingPartRange = run matchingPartRangeInput
                        matchingPartRange @ notMatchingPartRange
                    | S ->
                        let matchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s =
                                { min = part.s.min
                                  max = step.value - 1 } }

                        let notMatchingPartRangeInput: PartRange =
                            { value = step.output
                              x = { min = part.x.min; max = part.x.max }
                              m = { min = part.m.min; max = part.m.max }
                              a = { min = part.a.min; max = part.a.max }
                              s = { min = step.value; max = part.s.max } }

                        let notMatchingPartRange = runSteps (index + 1) notMatchingPartRangeInput
                        let matchingPartRange = run matchingPartRangeInput
                        matchingPartRange @ notMatchingPartRange

            runSteps 0 partRange

    let start =
        { value = "in"
          x = { min = 1; max = 4000 }
          m = { min = 1; max = 4000 }
          a = { min = 1; max = 4000 }
          s = { min = 1; max = 4000 } }

    run start

let combinations (partRange: PartRange) : Int128 =
    let x = partRange.x.max - partRange.x.min + 1
    let m = partRange.m.max - partRange.m.min + 1
    let a = partRange.a.max - partRange.a.min + 1
    let s = partRange.s.max - partRange.s.min + 1

    Int128.Parse(string x)
    * Int128.Parse(string m)
    * Int128.Parse(string a)
    * Int128.Parse(string s)

let part2 (input: string List) =
    let partRanges = getPartRanges (getWorkflows input)
    let accepted = partRanges |> List.filter (fun partRange -> partRange.value = "A")
    printfn "%A" accepted

    accepted |> List.map combinations |> List.sum |> string
