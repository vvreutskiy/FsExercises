#r @"packages/FAKE/Tools/FakeLib.dll"
open Fake

RestorePackages()

// Properties
let buildDir = "./build/"
let testDir  = buildDir

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir]
)

Target "BuildApp" (fun _ ->
   !! "**/*.fsproj"
    |> MSBuildRelease buildDir "Build"
    |> Log "AppBuild-Output: "
)

Target "Test" (fun _ ->
    !! (testDir + "/*Tests.dll")
      |> Fake.Testing.XUnit2.xUnit2 (fun p ->
          {p with
             ShadowCopy = false
             XmlOutputPath = Some (testDir + "TestResults.xml") 
             ToolPath = "packages/xunit.runner.console/tools/xunit.console.exe"})
)

Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)

// Dependencies
"Clean"
  ==> "BuildApp"
  ==> "Test"
  ==> "Default"

// start build
RunTargetOrDefault "Default"