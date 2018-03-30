module Cmd (buildRules, testRules, lintRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , phony
    , cmd
    , need
    )

prefix :: String -> String -> String
prefix prefix' s = prefix' ++ s

cmdTestFor :: String -> String
cmdTestFor x =
    "stack test " ++ x

cmdBuild :: String
cmdBuild =
    "stack build --copy-bins"

type Pkg = String
type Test = String

-- | The names of the packages.
pkgs :: [String]
pkgs =
    [ "hcoord"
    , "hcoord-utm"
    , "hcoord-irish"
    , "hcoord-mgrs"
    , "hcoord-osgb"
    ] 

-- | The pairs are names of the pkg and test.
testPkgs :: [(Pkg, Test)]
testPkgs =
    (\p -> (p, p ++ "-test")) <$> pkgs

lintRule :: String -> Rules ()
lintRule s =
    phony ("lint-" ++ s) $
        cmd
            (Cwd s) 
            Shell
            (cmdTestFor s ++ ":hlint-test")

lintRules :: Rules ()
lintRules = do
    _ <- sequence_ $ lintRule <$> pkgs

    phony "lint" $ need
        $ "lint-build"
        : (prefix "lint-" <$> pkgs)

testRule :: (Pkg, Test) -> Rules ()
testRule (pkg, test) =
    phony ("test-" ++ pkg) $
        cmd
            (Cwd pkg)
            Shell
            (cmdTestFor $ pkg ++ ":" ++ test)

testRules :: Rules ()
testRules = do
    _ <- sequence_ $ testRule <$> testPkgs
    phony "tests" $ need $ prefix "test-" . fst <$> testPkgs

buildRule :: Pkg -> Rules ()
buildRule s =
    phony s $
        cmd (Cwd s) Shell cmdBuild

buildRules :: Rules ()
buildRules = do
    _ <- sequence_ $ buildRule <$> pkgs
    phony "pkgs" $ need pkgs
