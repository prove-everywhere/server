{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad (join)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import Data.Monoid ((<>))

import Test.Hspec
import Test.Hspec.Wai
import qualified Test.Hspec.Wai as T

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
        v `shouldSatisfy` (> Just (8, 4, 0))

spec :: Spec
spec = with app $ do
    describe "/start" $ do
        it "responds 200" $ do
            post "/start" "" `shouldRespondWith` 200
        it "responds inifial_info (ID: 0)" $ do
            res <- post "/start" ""
            let info = decode $ simpleBody res
            liftIO $ fmap initialInfoId info `shouldBe` (Just 0)

    describe "/list" $ do
        it "responds nil when no coqtop" $ do
            res <- get "/list"
            let l = decode $ simpleBody res
            liftIO $ l `shouldBe` (Just [] :: Maybe [Coqtop])
        it "responds coqtop list" $ do
            post "/start" "" `shouldRespondWith` 200
            post "/start" "" `shouldRespondWith` 200
            post "/start" "" `shouldRespondWith` 200
            res <- get "/list"
            let l = decode $ simpleBody res
            liftIO $ l `shouldBe` (Just [coqtopOf 0, coqtopOf 1, coqtopOf 2])

    describe "/terminate" $ do
        it "responds empty object meaning success" $ do
            cId <- newCoqtop
            res <- T.delete ("/terminate/" <> showBS cId)
            let e = decode $ simpleBody res
            liftIO $ e `shouldBe` (Just EmptyObject)
        it "responds 404 when no such coqtop" $ do
            T.delete "/terminate/0" `shouldRespondWith` 404

    describe "/command" $ do
        it "responds info output when sending Require Import _" $ do
            cId <- newCoqtop
            let command = Command "Require Import ssreflect."
            res <- post ("/command/" <> showBS cId) (encode command)
            let output = decode $ simpleBody res
            liftIO $ output `shouldSatisfy` isJust
            liftIO $ output `shouldSatisfy` (isJust . join . fmap coqtopOutputLast)
            liftIO $ (fmap outputType . join . fmap coqtopOutputLast) output `shouldBe` (Just InfoOutput)
        it "responds proof output when sending Goal _" $ do
            cId <- newCoqtop
            let command = Command "Goal forall P Q : Prop, (P -> Q) -> P -> Q."
            res <- post ("/command/" <> showBS cId) (encode command)
            let output = decode $ simpleBody res
            liftIO $ output `shouldSatisfy` isJust
            liftIO $ output `shouldSatisfy` (isJust . join . fmap coqtopOutputLast)
            liftIO $ (fmap outputType . join . fmap coqtopOutputLast) output `shouldBe` (Just ProofOutput)
        it "responds error output when sending hoge" $ do
            cId <- newCoqtop
            let command = Command "hoge."
            res <- post ("/command/" <> showBS cId) (encode command)
            let output = decode $ simpleBody res
            liftIO $ output `shouldSatisfy` isJust
            liftIO $ output `shouldSatisfy` (isJust . join . fmap coqtopOutputError)
            liftIO $ (fmap outputType . join . fmap coqtopOutputError) output `shouldBe` (Just ErrorOutput)

  where
    coqtopOf n = Coqtop n undefined undefined undefined undefined undefined undefined
    showBS :: Show a => a -> BS.ByteString
    showBS = BS.pack . show
    newCoqtop = do
        res <- post "/start" ""
        let Just info = decode $ simpleBody res
        return $ initialInfoId info
