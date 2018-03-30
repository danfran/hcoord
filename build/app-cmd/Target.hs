module Target where

import Development.Shake (Rules)
import Cmd (buildRules, testRules, lintRules)

allWants :: [ String ]
allWants = ["pkgs", "tests"]

allRules :: Rules ()
allRules = do
    Target.buildRules
    Target.testRules

buildRules :: Rules ()
buildRules = do
    Cmd.buildRules

testRules :: Rules ()
testRules = do
    Cmd.testRules
    Cmd.lintRules
