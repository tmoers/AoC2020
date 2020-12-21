module AoC2020.Day08

open FParsec

type Instruction =
    | Acc of int
    | Jmp of int
    | Nop of int

type CpuState =
    { Accumulator : int
      ProgramCounter : int }

module CpuState =
    let initial =
        { Accumulator = 0
          ProgramCounter = 0 }

type History = History of int list

type ProgramState =
    | Finished of History * CpuState
    | InfiniteLoop of History * CpuState
    | Failed of History * CpuState * string

module ProgramState =
    let isInfiniteLoop = function
        | InfiniteLoop _ -> true
        | _ -> false
    let isFinished = function
        | Finished _ -> true
        | _ -> false
    let cpuState = function
        | Finished (_, s)
        | InfiniteLoop (_, s)
        | Failed (_, s, _) -> s

module History =
    let empty =
        History []
    let contains (History pcs) pc =
        pcs |> List.contains pc
    let cons (History pcs) pc =
        History (pc::pcs)
    let unwrap (History pcs) =
        pcs

let parse (instructionLine : string) =
    let number = ((pchar '+' >>. pint32) <|> (pchar '-' >>. pint32 |>> fun i -> -i)) |>> int
    let acc = pstring "acc" >>. pchar ' ' >>. number |>> Acc
    let jmp = pstring "jmp" >>. pchar ' ' >>. number |>> Jmp
    let nop = pstring "nop" >>. pchar ' ' >>. number |>> Nop
    let parser = acc <|> jmp <|> nop

    match run parser instructionLine with
    | Success (r, _, _) -> Result.Ok r
    | Failure (_, e, _) -> Result.Error (sprintf "Invalid instruction: %O" e)

let execInstruction instruction cpuState =
    match instruction with
    | Acc i ->
        { Accumulator = cpuState.Accumulator + i
          ProgramCounter = cpuState.ProgramCounter + 1 }
    | Jmp i ->
        { cpuState with
            ProgramCounter = cpuState.ProgramCounter + i }
    | Nop _ ->
        { cpuState with
            ProgramCounter = cpuState.ProgramCounter + 1 }

let stepProgram program cpuState =
    program
    |> Array.tryItem cpuState.ProgramCounter
    |> Result.ofOptionWith (fun _ -> sprintf "Program error, no instruction found at program counter [%i]" cpuState.ProgramCounter)
    |> Result.map (fun instruction -> execInstruction instruction cpuState)

let runProgram (program: Instruction array) =
    let rec run history cpuState =
        let isStartedLooping pc = History.contains history pc
        let isFinished pc = program.Length <= pc
        let updateHistory pc = History.cons history pc

        match stepProgram program cpuState with
        | Result.Ok nextCpuState when isStartedLooping nextCpuState.ProgramCounter ->
            InfiniteLoop (history, cpuState)
        | Result.Ok nextCpuState when isFinished nextCpuState.ProgramCounter ->
            Finished (history, cpuState)
        | Result.Ok nextCpuState ->
            run (updateHistory nextCpuState.ProgramCounter) nextCpuState
        | Result.Error e ->
            Failed (history, cpuState, e)

    run History.empty CpuState.initial

let findCandidateRepairings program =
    let rec findCandidateRepairings instructions1 instructions2 =
        seq {
            match instructions2 with
            | [] ->
                yield! Seq.empty
            | h::tail ->
                match h with
                | Nop i -> yield instructions1@(Jmp i)::tail
                | Jmp i -> yield instructions1@(Nop i)::tail
                | Acc _ -> ()
                yield! findCandidateRepairings (instructions1@[h]) tail
        }
    findCandidateRepairings [] program

let solveA resourcesPath =
    Data.readLinesAs parse (System.IO.Path.Join(resourcesPath, "day08.input.txt"))
    |> Result.sequence
    |> Result.map Array.ofList
    |> Result.map runProgram
    |> Result.ifErrorWith (fun e -> Failed (History.empty, CpuState.initial, e))

let solveB resourcesPath =
    Data.readLinesAs parse (System.IO.Path.Join(resourcesPath, "day08.input.txt"))
    |> Result.sequence
    |> Result.map findCandidateRepairings
    |> Result.map (Seq.map (Array.ofList >> runProgram))
    |> Result.map (Seq.find ProgramState.isFinished)