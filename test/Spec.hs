import Test.Hspec
import OSGeo.Proj4

main :: IO ()
main = do
  Right pj <- createProjection
    "+proj=latlong +ellps=clrk66"
    "+proj=merc +ellps=clrk66 +lat_ts=33"

  Right pj2 <- createProjection
    "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
    "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  hspec (spec pj pj2)

spec :: Projection -> Projection -> Spec
spec pj pj2 = describe "OSGeo.Proj4" $ describe "transform" $ do
  it "behaves as model" $ do
    transform pj (-16 :: Double, 20.25 :: Double)
        `shouldBe` Just (-1495284.2114734806, 1920596.789917442)

  -- https://spatialreference.org/ref/epsg/27700/
  -- https://epsg.io/transform#s_srs=27700&t_srs=4326&x=0.0000000&y=0.0000000
  it "behaves as model" $ do
    transform pj2 (0 :: Double, 0 :: Double)
        `shouldBe` Just (-7.557159806905192, 49.766807235142615)
