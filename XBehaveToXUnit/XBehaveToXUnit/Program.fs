

// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open Argu
open XBehaveToXUnit
open FSharpPlus

printfn "Starting Migrating"

let rec findDirectories folderPath =
    seq {
        yield folderPath
        yield! Directory.GetDirectories(folderPath) |> Seq.collect findDirectories
    }

let rec findSpecsRecursively folderPath =
    folderPath
    |> findDirectories
    |> Seq.collect (fun d -> Directory.GetFiles(d, "*.cs"))
    |> Seq.filter (fun f -> File.ReadAllText(f) |> String.isSubString "Scenario")

let migrateFile path =
    printfn $"migrating {path}"
    let original = System.IO.File.ReadAllText path
    let migrated = Migrator.migrate original
    System.IO.File.WriteAllText(path, migrated)
    printfn $"done migrating {path}"

type Arguments =
    | File of string
    | Folder of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ -> "specify a file."
            | Folder _ -> "specify a folder."

let argv = System.Environment.GetCommandLineArgs() |> Array.skip 1

("", argv) ||> Array.fold (fun a v -> a + " " + v) |> fun x -> printfn $"{x}"

let parser =  ArgumentParser.Create<Arguments>(programName = "XBehaveToXUnit")
// pass the reader to the Parse call
let results = parser.Parse argv

let file = results.TryGetResult File
let folder = results.TryGetResult Folder

match folder, file with
| Some folder, _ ->
    printfn $"migrating folder {folder}"
    let specs = findSpecsRecursively folder
    let count = specs |> Seq.length
    printfn $"migrating {count} files"
    specs |> Seq.iteri (fun i path ->
        migrateFile path
        printfn $"migrated {i + 1}/{count}")
    printfn $"done migrating folder {folder}"
| _, Some path ->
    migrateFile path
| _, _ ->
    let usage = parser.PrintUsage()
    printfn $"{usage}"
