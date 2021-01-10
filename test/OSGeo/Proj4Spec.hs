module OSGeo.Proj4Spec (spec, main) where

import Control.Monad.IO.Class (liftIO)

import Test.Hspec
import OSGeo.Proj4

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "OSGeo.Proj4" $ do
    describe "projection" $
      it "returns Right x for correct definition" $ do
        proj <- liftIO $ projection "+proj=latlong +ellps=clrk66"
        isRight proj `shouldBe` True

    describe "Projection" $
      it "implements Show" $ do
        proj <- liftIO $ projection "+proj=latlong +ellps=clrk66"
        show proj
          `shouldBe` "Right \"+proj=latlong +ellps=clrk66\""

    describe "transform" $
      it "behaves as model" $ do
        Right pjMerc <- projection "+proj=merc +ellps=clrk66 +lat_ts=33"
        Right pjLatLong <- projection "+proj=latlong +ellps=clrk66"

        transform pjLatLong pjMerc (toRadian (-16), toRadian 20.25)
          `shouldBe` Just ((-1495284.2114734803), 1920596.789917442)

    -- https://spatialreference.org/ref/epsg/27700/
    -- https://epsg.io/transform#s_srs=27700&t_srs=4326&x=0.0000000&y=0.0000000
    describe "transform" $
      it "behaves as model" $ do
        Right pjMerc    <- projection "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
        Right pjLatLong <- projection "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

        transform pjMerc pjLatLong (0 :: Double, 0 :: Double)
          `shouldBe` Just (-0.13189732134383778,0.868594644401911)

    describe "isLatLong" $ do
      it "returns True for latlong" $ do
        Right pj <- projection "+proj=latlong +ellps=clrk66"
        isLatLong pj `shouldBe` True

      it "returns False for utm" $ do
        Right pj <- projection "+proj=utm +zone=30 +ellps=intl +units=m +no_defs"
        isLatLong pj `shouldBe` False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
