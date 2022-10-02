namespace Diagrammatic.Core.Tests

/// These are sample Unit tests that show you  briefly how you can use
/// [Expecto](https://github.com/haf/expecto) unit test library
module Sample =
    open Expecto
    open Diagrammatic.Core.Say
    open Diagrammatic.PHOTypes
    open Diagrammatic.Fix

    //let onetwof = function 
    //| branchesin -> 
    //    seq {
    //        Diagrammatic.PHOGraph.Cons (1, PHOGraph.In ( StreamBase.Inj <| 
    //                Cons (2, (PHOGraph.Ref (Seq.head branchesin)))
    //        )) |> StreamBase.Inj
    //    } 
//
    //let onetwo = PHOGraph.Mu onetwof


    [<Tests>]
    let tests =
        testList "samples"
            [ 
                test "fix fact'" {
                    let fact' f n = if n = 0 then 1 else n * ((force f) (n - 1))
                    let fact = Diagrammatic.Fix.fix fact'
                    Expect.equal [1; 1; 2; 6; 24; 120; 720] (List.map fact [0;1;2;3;4;5;6]) "fix fact' is fact"
                }
                //test "Hello, World!" {
              //    /// Testing our Core Library
              //    let actual = hello "World!"
              //    Expect.equal actual "Hello, World!" "hello Should say Hello, World!"
              //}
              ///// Expecto Tests sample https://github.com/haf/expecto#expecto
              //testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
              //    let subject = true
              //    Expect.isTrue subject "I compute, therefore I am."
//
              //testCase "when true is not (should fail)" <| fun _ ->
              //    let subject = false
              //    Expect.isTrue subject "I should fail because the subject is false"
//
              //testCase "I'm skipped (should skip)" <| fun _ -> Tests.skiptest "Yup, waiting for a sunny day..."
//
              //testCase "I'm always fail (should fail)" <| fun _ -> Tests.failtest "This was expected..."
//
              //testCase "contains things"
              //<| fun _ -> Expect.containsAll [| 2; 3; 4 |] [| 2; 4 |] "This is the case; {2,3,4} contains {2,4}"
//
              //testCase "contains things (should fail)"
              //<| fun _ -> Expect.containsAll [| 2; 3; 4 |] [| 2; 4; 1 |] "Expecting we have one (1) in there"
//
              //testCase "Sometimes I want to ༼ノಠل͟ಠ༽ノ ︵ ┻━┻"
              //<| fun _ -> Expect.equal "abcdëf" "abcdef" "These should equal"
//
              //test "I am (should fail)" { "╰〳 ಠ 益 ಠೃ 〵╯" |> Expect.equal true false } 
              ]
