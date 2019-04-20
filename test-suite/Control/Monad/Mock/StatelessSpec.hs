{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Mock.StatelessSpec (spec) where

import Prelude hiding (readFile, writeFile)

import Control.Exception (evaluate)
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.ST (runST)
import Data.Function ((&))
import Test.Hspec

import Control.Monad.Mock.Stateless
import Control.Monad.Mock.TH

class MonadError e m => MonadFileSystem e m | m -> e where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
makeAction "FileSystemAction" [ts| MonadFileSystem String |]

copyFileAndReturn :: MonadFileSystem e m => FilePath -> FilePath -> m String
copyFileAndReturn a b = do
  x <- readFile a
  writeFile b x
  return x

spec :: Spec
spec = describe "MockT" $ do
  it "runs computations with mocked method implementations" $ do
    let result = runST
          $ copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ ReadFile "foo.txt" :-> pure "file contents"
                     , WriteFile "bar.txt" "file contents" :-> pure () ]
          & runExceptT
    result `shouldBe` Right "file contents"

  it "raises an exception if calls are not in the right order" $ do
    let result = runST
          $ copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ WriteFile "bar.txt" "file contents" :-> pure ()
                     , ReadFile "foo.txt" :-> pure "file contents" ]
          & runExceptT
        exnMessage =
          "runMockT: argument mismatch in readFile\n\
          \  given: ReadFile \"foo.txt\"\n\
          \  expected: WriteFile \"bar.txt\" \"file contents\"\n"
    evaluate result `shouldThrow` errorCall exnMessage

  it "raises an exception if calls are missing" $ do
    let result = -- running on top of IO
            copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ ReadFile "foo.txt" :-> pure "file contents"
                     , WriteFile "bar.txt" "file contents" :-> pure ()
                     , ReadFile "qux.txt" :-> pure "file contents 2" ]
          & runExceptT
    let exnMessage =
          "runMockT: expected the following unexecuted actions to be run:\n\
          \  ReadFile \"qux.txt\"\n"
    result `shouldThrow` errorCall exnMessage

  it "raises an exception if there are too many calls" $ do
    let result = runST
          $ copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ ReadFile "foo.txt" :-> pure "file contents" ]
          & runExceptT
        exnMessage =
          "runMockT: expected end of program, called writeFile\n\
          \  given action: WriteFile \"bar.txt\" \"file contents\"\n"
    evaluate result `shouldThrow` errorCall exnMessage

  it "SkipWhile" $ do
    let result = runST
          $ copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ SkipWhile (\case ReadFile{} -> Just (pure "skipped") ; _ -> Nothing)
                     , (WriteFile "bar.txt" "skipped" :-> pure ()) ]
          & runExceptT
    result `shouldBe` Right "skipped"

  it "Match" $ do
    let result = runST
          $ copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ Match "ReadFile" (\case ReadFile{} -> Just (pure "match") ; _ -> Nothing)
                     , WriteFile "bar.txt" "match" :-> pure () ]
          & runExceptT
    result `shouldBe` Right "match"

  it "Either" $ do
    let result = runST
          $ copyFileAndReturn "foo.txt" "bar.txt"
          & runMockT [ Either ZeroMatch (ReadFile "foo.txt" :-> pure "file contents")
                     , Either (WriteFile "bar.txt" "file contents" :-> pure ()) ZeroMatch ]
          & runExceptT
    result `shouldBe` Right "file contents"

