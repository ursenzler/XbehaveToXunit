module XBehaveToXUnit.Migrator

open System.Text.RegularExpressions
open FSharpPlus

let addTasks o =
    if (o |> String.isSubString "using System.Threading.Tasks;") then o
        else o |> String.replace "using Xbehave;" "using Xbehave;\r\n    using System.Threading.Tasks;"

let addXunit o =
    if (o |> String.isSubString "using Xunit;") then o
        else o |> String.replace "using Xbehave;" "using Xbehave;\r\n    using Xunit;"

let replaceScenario =
    String.replace "[Scenario]" "[Fact]"

let factNameAndArguments o =
    let argumentMatches = Regex.Matches(o, "\[Scenario\]\s*public void \w+\(\s*(?<arguments>[\w\[\]\s,?<>]*)\)\s*{")
    let mutable x = Regex.Replace(o, "\[Scenario\]\s*public void (?<name>\w+)\(\s*(?<arguments>[\w\[\]\s,?<>]*)\)\s*{", "[Fact]\r\n        public async Task ${name}()\r\n        {\r\n            ${arguments}")

    for m in argumentMatches do
        let arguments = m.Groups["arguments"].Value
        let newArguments = arguments |> String.replace ",\r\n" ";\r\n" |> fun s -> if s.Length > 0 then s + ";\r\n" else s

        x <- x |> String.replace arguments newArguments

    x

type CountArgumentsState = Normal | InsideArray | InsideString
let countArguments (arguments : string) =
    ((Normal, 0), arguments)
    ||> Seq.fold (fun (state, count) c ->
        match state with
        | Normal ->
            match c with
            | ',' ->
                match count with
                | 0 -> (Normal, 2)
                | _ -> (Normal, count + 1)
            | '{' -> (InsideArray, count)
            | '"' -> (InsideString, count)
            | _ -> (Normal, count)
        | InsideArray ->
            match c with
            | '}' -> (Normal, count)
            | _ -> (InsideArray, count)
        | InsideString ->
            match c with
            | '"' -> (Normal, count)
            | _ -> (InsideString, count))
    |> snd

let findNthComma n (arguments : string) =
    let mutable found = 0
    let mutable index = 0
    arguments
    |> Seq.iteri (fun i c ->
        match c with
        | ',' ->
            found <- found + 1
            if (found = n) then
                index <- i
        | _ -> ()
       )
    index

type EscapingState = Normal | InsideString
let escapeClosingBracketInStrings (o : string) =
    (("", EscapingState.Normal), o)
    ||> fold (fun (value, state) c ->
        match state with
        | Normal ->
            match c with
            | '"' -> (value + "\"", EscapingState.InsideString)
            | _ -> (value + $"{c}", state)
        | InsideString ->
            match c with
            | '"' -> (value + "\"", EscapingState.Normal)
            | '[' -> (value + "°o", state)
            | ']' -> (value + "°c", state)
            | _ -> (value + $"{c}", state)
        )
    |> fst
    |> String.replace "[]" "°o°c"

let deEscapeClosingBracketInStrings (o : string) =
    o |> String.replace "°c" "]" |> String.replace "°o" "["


let factNameAndArgumentsOfExamples o =
    let escaped = escapeClosingBracketInStrings o

    let matches = Regex.Matches(escaped, "\[Scenario\]\s*(?<examples>(\[Example\([\w\s\{\},?<>=\".\/\+\-\)\(:°]*\]\s*)+)public void (?<method>\w+)\(\s*(?<arguments>[\w\s\{\},?<>=\".\/\+\-\\[\]°)\(:]*)\)\s*{(?<body>[\w\s=.:;?<>=\r\n\"\(\)>$\{\},üäö<\\\/°\[\]\-]*)        }")
    let mutable x = escaped

    for m in matches |> Seq.rev do // rev because we need to change the file from the end. Otherwise the indices are wrong
        let examples = m.Groups["examples"].Value
        let firstExampleMatch = Regex.Match(examples, "\[Example\((?<arguments>[\w\s\{\}\[°,?<>=\"./+\-\)\(:]*)\)\]\r\n")
        let exampleArguments = firstExampleMatch.Groups["arguments"].Value
        let numberOfArguments = countArguments exampleArguments

        let arguments = m.Groups["arguments"].Value
        let cutAt = arguments |> findNthComma numberOfArguments
        let newArguments = arguments |> String.truncate cutAt
        let newLocals = arguments |> String.drop (cutAt + 1) |> String.replace ",\r\n" ";\r\n" |> fun s -> if s.Length > 0 then s + ";\r\n" else s

        let methodName = m.Groups["method"].Value;
        let body = m.Groups["body"].Value
        let newBody = newLocals + body

        let mutable theory = m.Value;
        theory <- theory |> String.replace arguments newArguments
        theory <- theory |> String.replace body newBody
        theory <- theory |> String.replace ("void " + methodName) ("async Task " + methodName)

        x <- x |> String.replace m.Value theory
    x |> deEscapeClosingBracketInStrings


let steps =
    let asyncSteps a =
        Regex.Replace(a, "\${0,1}\"(?<text>.*)\"\.x\(async \(\)\s*=>\s*", "// ${text}\r\n            ")
    let syncSteps s =
        Regex.Replace(s, "\${0,1}\"(?<text>.*)\"\.x\(\(\)\s*=>\s*", "// ${text}\r\n            ")

    let removeClosingParenthesisAsync x =
        Regex.Replace(x, "(?<content>\${0,1}\"(?<text>.*)\"\.x\(async \(\)\s*=>[\w\s=.:\r\n\"\(\)>$\{\},üäö+<\[\]-]*)\);", "${content};")
    let removeClosingParenthesisSync x =
        Regex.Replace(x, "(?<content>\${0,1}\"(?<text>.*)\"\.x\(\(\)\s*=>[\w\s=.:\r\n\"\(\)>$\{\},üäö+<\[\]-]*)\);", "${content};")

    removeClosingParenthesisAsync >> removeClosingParenthesisSync >> asyncSteps >> syncSteps

let examples o =
    o |> String.replace "[Example(" "[InlineData("

let theory o =
    o |> String.replace "[Scenario]\r\n        [Example" "[Theory]\r\n        [Example"

let orderUsings o =
    let namespaceMatches = Regex.Matches(o, "using (?<namespace>[\w.]+);")

    let namespaces =
        namespaceMatches
        |> Seq.map (fun m -> m.Groups["namespace"].Value)

    let systemNamespaces = namespaces |> Seq.filter (fun n -> n |> String.startsWith "System")
    let otherNamespaces = namespaces |> Seq.filter (fun n -> n |> String.startsWith "System" |> not && n |> String.startsWith "Xbehave" |> not)

    let sortedSystemNamespaces = systemNamespaces |> Seq.sort
    let sortedOtherNamespaces = otherNamespaces |> Seq.sort

    let sortedNamespaces =
       sortedOtherNamespaces |> Seq.append sortedSystemNamespaces
        |> Seq.map (fun n -> $"using {n};")
        |> String.concat "\r\n    "

    Regex.Replace(o, "(using .*;\s*)*public class", sortedNamespaces + "\r\n\r\n    public class")


let addMissingAwaits =
    let addMissingAwaitsBeforeFactory x =
        Regex.Replace(x, "(?<!await) this.factory.(?<kind>Modules|Queries|Operations|RejectOperation|Storage.\w+.Persist\()", " await this.factory.${kind}")
    let addMissingAsyncInForEachAsync x =
        Regex.Replace(x, "(?<variable>[\w+\(\)])\s*.ForEachAsync\((?!async)", "${variable}.ForEachAsync(async ")
    let addMissingAwaitsInForEachAsync x =
        Regex.Replace(x, "(?<!await) (?<variable>\w+)\s*.ForEachAsync\(", " await ${variable}.ForEachAsync(")
    let addMissingAwaitsInSelectAsync x =
        Regex.Replace(x, "(?<!await) (?<variable>\w+)\s*.SelectAsync\(", " await ${variable}.SelectAsync(")

    addMissingAwaitsBeforeFactory >> addMissingAsyncInForEachAsync >> addMissingAwaitsInForEachAsync >> addMissingAwaitsInSelectAsync

let replaceDoubleSemicolons o =
    Regex.Replace(o, ";\s*;", ";")

let setEmployeeContainerDefaults o =
    Regex.Replace(o, "EmployeeContainer (?<variable>\w*);", "EmployeeContainer ${variable} = default; // generated by migration")

let replaceSetup =
    let employeeAndApplicant x =
        Regex.Replace(
            x,
            "Setup.Using\(\(\) => this.factory,\s*(?<setupDate>\w+)\)\s*.Employee\(\w+ => (?<employee>\w+) = \w+\)\s*.Applicant\(\w+ => (?<applicant>\w+) = \w+\);",
            "${employee} = await this.factory.SetupEmployee().WithEntryDate(${setupDate}).Setup();\r\n            ${applicant} = await this.factory.SetupApplicant(${setupDate});")
    let employee x =
        Regex.Replace(
            x,
            "Setup.Using\(\(\) => this.factory,\s*(?<setupDate>\w+)\)\s*.Employee\(\w+ => (?<employee>\w+) = \w+\);",
            "${employee} = await this.factory.SetupEmployee().WithEntryDate(${setupDate}).Setup();")
    let applicant x =
        Regex.Replace(
            x,
            "Setup.Using\(\(\) => this.factory,\s*(?<setupDate>\w+)\)\s*.Applicant\(\w+ => (?<applicant>\w+) = \w+\);",
            "${applicant} = await this.factory.SetupApplicant(${setupDate});")

    employeeAndApplicant >> employee >> applicant


let replaceCheck =
    let check x = Regex.Replace(x, "Check.Using\(\(\) => this.factory\)", "await CheckAsync.Using(this.factory)")
    let workday x =
        Regex.Replace(
            x,
            ".ForChangedWorkdays\(\s*\(\) => (?<employee>[ \w.<>\(\)]*),\s*(?<range>[ \w.<>\(\)]*),\s*(?<instant>[ \w.<>\(\)]*)\)",
            ".ForChangedWorkdays(${employee}, ${range}, ${instant})")
    let application x =
        Regex.Replace(
            x,
            ".ForApplication\(\s*\(\) => (?<applicant>[ \w.<>\(\)]*),\s*(?<instant>[ \w.<>\(\)]*)\)",
            ".ForApplication(${applicant}, ${instant})")
    let representation x =
        Regex.Replace(
            x,
            ".OperationRepresentation\((?<part1>[\s\w.<>\(\),]*)\(\) => (?<part2>[\s\w.<>\(\),]*)\(\) => ",
            ".OperationRepresentation(${part1}${part2}")

    check >> workday >> application >> representation

let migrate original =
    original
    |> addTasks
    |> addXunit
    |> factNameAndArgumentsOfExamples
    |> factNameAndArguments
    |> theory
    |> replaceScenario
    |> examples
    |> steps
    |> orderUsings
    |> addMissingAwaits
    |> replaceDoubleSemicolons
    |> replaceSetup
    |> replaceCheck