{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module M2X.DeviceAPISpec where

import Test.Hspec
import Test.QuickCheck
import M2X.DeviceAPI
import M2X.DeviceTypes
import Data.Aeson
import Data.String (fromString)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.

main :: IO ()
main = hspec spec

spec = 
    context "general" $ do
        let d10 = 10 :: Double
            d20 = 20 :: Double
            d70 = 70 :: Double
            i10 = 10 :: Int
            i20 = 20 :: Int

        describe "toJSON generates correct JSON data for" $ do
            let gt = StreamGt 10
                eq = StreamEq 10
                lt = StreamLt 20
                lte = StreamLte 70
                match = StreamMatch "Madrid"
                point = LocationPoint 10 20

            it "MultipleFilterOp" $
                toJSON gt `shouldBe` object ["gt" .= d10]
            it "SingleFilterOp" $
                toJSON eq `shouldBe` object ["eq" .= d10]
            it "StreamMultipleFilter" $
                toJSON (StreamMultipleFilter "temperature" [gt, lt]) `shouldBe` 
                  object ["temperature" .= object ["gt" .= d10, "lt" .= d20]]
            it "StreamSingleFilter" $
                toJSON (StreamSingleFilter "temperature" eq) `shouldBe` 
                  object ["temperature" .= object ["eq" .= d10]]
            it "StreamFilter" $ do
                let singleFilters = 
                        [ StreamSingleFilter "location" match
                        , StreamSingleFilter "hour" eq ]
                    multipleFilters = 
                        [ StreamMultipleFilter "temperature" [gt, lt]
                        , StreamMultipleFilter "speed" [gt, lte]]
                    expectation = object ["streams" .= object
                        [ "temperature" .= object ["gt" .= d10, "lt" .= d20]
                        , "speed"       .= object ["gt" .= d10, "lte" .= d70]
                        , "location"    .= object ["match" .= ("Madrid" :: String)]
                        , "hour"        .= object ["eq" .= d10]]]
                    aeson = toJSON (StreamFilter multipleFilters singleFilters)
                aeson `shouldBe` expectation
            it "LocationPoint" $
                toJSON point `shouldBe` object ["latitude" .= d10, "longitude" .= d20]
            it "LocationWithinCircle" $
                toJSON (LocationWithinCircle point Km 10) `shouldBe`
                  object ["location" .= 
                      object ["within_circle" .= 
                          object [ "center" .= object [ "latitude" .= d10, "longitude" .= d20]
                                                      , "radius" .= object ["km" .= d10]]]]
            it "LocationWithinPolygon" $
                toJSON (LocationWithinPolygon [point, LocationPoint 10 70, LocationPoint 20 70])
                  `shouldBe`
                      object ["location" .= 
                          object ["within_polygon" .= [ object ["latitude" .= d10, "longitude" .= d20]
                                                      , object ["latitude" .= d10, "longitude" .= d70]
                                                      , object ["latitude" .= d20, "longitude" .= d70]]]]
            it "NoLocation" $
                toJSON NoLocation `shouldBe` object ["location" .= ("none" :: String)]

        describe "ParseJSON correctly for" $ do
            it "TagResult" $
                decode "{\"tag #1\": 10}" `shouldBe` Just(TagResult "tag #1" i10)
            it "TagResultList" $
                decode "{ \"tags\": [{\"tag #1\": 10}, {\"tag #2\": 20}] }" `shouldBe`
                    Just (TagResultList [TagResult "tag #1" i10, TagResult "tag #2" i20])
      {-describe "StreamFilter" $ do-}
          {-it "Generates correct JSON data" $-}
              {-toJSON (StreamFilter ("temperature", StreamEq 12)) `shouldBe` object ["temperature" .= object ["eq" .= (12 ::Double)]]-}
