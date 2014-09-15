module OSGeo.Proj4Spec (spec, main) where

import Test.Hspec
import OSGeo.Proj4


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "OSGeo.Proj4" $ do
    describe "projection" $
        it "returns Right x for correct definition" $
            isRight (projection "+proj=latlong +ellps=clrk66")
                `shouldBe` True
    describe "Projection" $
        it "implements Show" $
            show (projection "+proj=latlong +ellps=clrk66")
                `shouldBe` "Right \"+proj=latlong +ellps=clrk66\""
    describe "transform" $
        it "behaves as model" $
            let Right pjMerc    = projection "+proj=merc +ellps=clrk66 +lat_ts=33"
                Right pjLatLong = projection "+proj=latlong +ellps=clrk66"
            in transform pjLatLong pjMerc (toRadian (-16), toRadian 20.25)
                `shouldBe` Just ((-1495284.2114734803), 1920596.789917442)
    describe "isLatLong" $ do
        it "returns True for latlong" $
            let Right  pj = projection "+proj=latlong +ellps=clrk66"
            in isLatLong pj `shouldBe` True
        it "returns False for utm" $
            let Right pj = projection "+proj=utm +zone=30 +ellps=intl +units=m +no_defs"
            in isLatLong pj `shouldBe` False



isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
