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
import ProveEverywhere.Coqtop

main :: IO ()
main = hspec $ do
    coqtop
    spec

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

coqtop :: Spec
coqtop = describe "coqtop (> 8.4)" $ do
    it "should be installed" $ do
        v <- getCoqtopVersion
        v `shouldSatisfy` isJust
        v `shouldSatisfy` (\v' -> v' > Just (8, 4, 0))

spec :: Spec
spec = with app $ do
    describe "/start" $ do
        it "responds 200" $ do
            post "/start" "" `shouldRespondWith` 200
        it "responds with ID: 0" $ do
            res <- post "/start" ""
            let info = decode $ simpleBody res
            liftIO $ info `shouldSatisfy` isJust
            liftIO $ fmap initialInfoId info `shouldBe` (Just 0)
