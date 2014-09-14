{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import Data.Aeson (decode)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)

import Test.Hspec
import Test.Hspec.Wai

import Network.Wai (Application)
import Network.Wai.Test (simpleBody)

import ProveEverywhere.Types
import ProveEverywhere.Server

main :: IO ()
main = hspec spec

app :: IO Application
app = do
    coqtopMap <- newMVar HM.empty
    seed <- newMVar 0
    return $ server config coqtopMap seed
  where
    config = Config
        { configPort = 0
        , configMaxNumProcs = Nothing
        , configKillTime = Nothing
        }

spec :: Spec
spec = with app $ do
    describe "/start" $ do
        it "responds 200" $ do
            post "/start" "" `shouldRespondWith` 200
        it "responds with ID: 0" $ do
            res <- post "/start" ""
            let info = decode $ simpleBody res
            liftIO $ info `shouldSatisfy` isJust
            let Just i = info
            liftIO $ initialInfoId i `shouldBe` 0
