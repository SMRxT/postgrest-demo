{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Lens
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.URL
import           System.IO
import           System.IO.Unsafe
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit               as HUnit
import           Test.Tasty.QuickCheck          as QC

data TestEnv
   = TestEnv !Connection !(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

main :: IO ()
main = defaultMain
   $ withResource initEnv freeEnv tests

initEnv :: IO TestEnv
initEnv = do
   pgConn <- connect $ fromJust
      $ parseDatabaseUrl "postgres://localhost:5432/postgrest_demo"

   depProc@(_, _, _, depPH) <- createProcess
      (shell "cd ../db && sqitch deploy")
   depECode <- waitForProcess depPH
   print depECode

   verProc@(_, _, _, verPH) <- createProcess
      (shell "cd ../db && sqitch verify")
   verECode <- waitForProcess verPH
   print verECode

   wsProc@(_, _, _, wsPH) <- createProcess $
      (shell "postgrest postgres://andrew:@localhost:5432/postgrest_demo -a postgrest_anonymous -s postgrest")
         { std_in  = UseHandle stdin
         , std_out = UseHandle stdout
         , std_err = UseHandle stderr
         }

   return $ TestEnv pgConn wsProc

freeEnv :: TestEnv -> IO ()
freeEnv (TestEnv pgConn (_, _, _, wsPH)) = do
   terminateProcess wsPH

   revProc@(_, _, _, revPH) <- createProcess
      (shell "cd ../db && sqitch revert -y")
   revECode <- waitForProcess revPH
   print revECode

--

tests :: IO TestEnv -> TestTree
tests getEnv = testGroup "HTTP Tests"
   [
   ]
