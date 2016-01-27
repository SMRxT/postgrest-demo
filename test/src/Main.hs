{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Lens
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe
import           Data.String
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Typeable
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.URL
import           GHC.Generics
import           Network.Wreq
import           Network.Wreq.Types             (Postable (..))
import           System.IO
import           System.IO.Unsafe
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit               as HUnit
import           Test.Tasty.QuickCheck          as QC

type Account = Text

data TestEnv
   = TestEnv
      { envPostgresConnection :: !Connection
      , envWebserverHandles   :: !(Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
      , envAccountSet         :: MVar (Map Text Account)
      }

-- BEGIN: Messages

data PostgrestError
   = PostgrestError
      { pgerrCode    :: Text
      , pgerrDetails :: Maybe Text
      , pgerrHint    :: Maybe Text
      , pgerrMessage :: Text
      }
   deriving (Show, Typeable, Generic)

instance ToJSON PostgrestError where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON PostgrestError where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

--

data RegisterPostReq
   = RegisterPostReq
      { regpostreqEmail    :: Text
      , regpostreqName     :: Text
      , regpostreqPassword :: Text
      }
   deriving (Show, Typeable, Generic)

instance ToJSON RegisterPostReq where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON RegisterPostReq where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase
instance Postable RegisterPostReq where
   postPayload = postPayload . toJSON

--

data RegisterPostRes
   = RegisterPostRes
      { regpostresRegisterAccount :: Text
      }
   deriving (Show, Typeable, Generic)

instance ToJSON RegisterPostRes where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON RegisterPostRes where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

$(makeLensesWith abbreviatedFields ''PostgrestError)
$(makeLensesWith abbreviatedFields ''RegisterPostReq)
$(makeLensesWith abbreviatedFields ''RegisterPostRes)

-- END: Messages

$(makeLensesWith abbreviatedFields ''TestEnv)

main :: IO ()
main = defaultMain $ withResource initEnv (freeEnv True) tests

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

   threadDelay 500000

   as <- newMVar Map.empty
   return $ TestEnv pgConn wsProc as

freeEnv :: Bool -> TestEnv -> IO ()
freeEnv revert (TestEnv pgConn (_, _, _, wsPH) aset) = do
   terminateProcess wsPH

   when revert $ do
      revProc@(_, _, _, revPH) <- createProcess
         (shell "cd ../db && sqitch revert -y")
      revECode <- waitForProcess revPH
      print revECode

      as <- readMVar aset
      mapM_ (execute_ pgConn)
         ((\(_, a) -> fromString $ "DROP ROLE " ++ T.unpack a ++ ";") <$> Map.toList as)

--

tests :: IO TestEnv -> TestTree
tests getEnv = testGroup "HTTP Tests"
   [ testCase "Register User" $ caseRegisterUser getEnv
   , testCase "Get Account info as Anon" $ caseUnauthenticatedAccount getEnv
   ]

caseRegisterUser :: IO TestEnv -> IO ()
caseRegisterUser getEnv = do
   env <- getEnv
   res <- post "http://localhost:3000/rpc/register_account"
      (RegisterPostReq "andrew.rademacher@smrxt.com" "Andrew Rademacher" "12345")
   case res ^? responseBody . nth 0 . _JSON . (registerAccount :: Lens' RegisterPostRes Text) of
      Nothing -> assertFailure "Response did not contain account name."
      Just  a -> modifyMVar_ (env ^. accountSet) (return . Map.insert "andrew" a)

caseUnauthenticatedAccount :: IO TestEnv -> IO ()
caseUnauthenticatedAccount getEnv = do
   env <- getEnv
   res <- flip getWith "http://localhost:3000/account"
            $ defaults & checkStatus ?~ (\_ _ _ -> Nothing)
   case res ^? responseBody . _JSON . (code :: Lens' PostgrestError Text) of
      Nothing -> assertFailure "Error was not produced."
      Just  a -> "42501" @=? a
