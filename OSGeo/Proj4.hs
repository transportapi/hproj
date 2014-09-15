{-# LANGUAGE MultiParamTypeClasses
           , CPP
           , ForeignFunctionInterface
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           #-}
module OSGeo.Proj4 (
    Projection
  , Projectable (transform)
  , projection
  , isLatLong
  , toRadian
  , fromRadian
) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

data ProjectionPtr
newtype Projection = Projection {unProjection :: ForeignPtr ProjectionPtr}

instance Show Projection where
    show (Projection fPtr)
        = unsafePerformIO $ fmap (\s -> "\"" ++ tail s ++ "\"") $
            peekCString =<< (withForeignPtr fPtr $ flip c_pjGetDef 0)

withProjectionPtr :: Projection -> (Ptr ProjectionPtr -> IO a) -> IO a
withProjectionPtr = withForeignPtr . unProjection
        
foreign import ccall "proj_api.h pj_init_plus" c_pjInitPlus  ::
    CString -> IO (Ptr ProjectionPtr)

foreign import ccall "proj_api.h pj_get_def" c_pjGetDef  ::
    Ptr ProjectionPtr -> CInt -> IO CString

foreign import ccall "proj_api.h pj_transform" c_pjTransform  ::
    Ptr ProjectionPtr -> Ptr ProjectionPtr -> CInt -> CInt
    -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt

foreign import ccall "proj_api.h pj_is_latlong" c_pjIsLatLong  ::
    Ptr ProjectionPtr -> IO CInt

foreign import ccall "proj_api.h &pj_free" c_pjFree  ::
    FunPtr (Ptr ProjectionPtr -> IO ())

projection :: String -> Either String Projection
projection proj = unsafePerformIO $ withCString proj $ \cProj -> do
    ptr <- c_pjInitPlus cProj
    if (ptr==nullPtr)
        then return $ Left $ "projection: could not initialize projection " ++
                             "'" ++ proj ++ "'"
        else fmap (Right . Projection) $ newForeignPtr c_pjFree ptr

    
class Projectable a where
    transform :: Projection -> Projection -> a -> Maybe a

instance Projectable (Double, Double) where
    transform pjFrom pjTo (x,y) = unsafePerformIO $
        withProjectionPtr pjFrom $ \pjFrom' ->
        withProjectionPtr pjTo $ \pjTo' ->
        alloca $ \x' ->
        alloca $ \y' -> do
            poke x' $ convertDouble x
            poke y' $ convertDouble y
            err <- c_pjTransform  pjFrom' pjTo' 1 1 x' y' nullPtr
            case err of
                0 -> do
                    x'' <- fmap convertDouble $ peek x'
                    y'' <- fmap convertDouble $ peek y'
                    return $ Just (x'',y'')
                _ -> return Nothing

instance Projectable (Double, Double, Double) where
    transform pjFrom pjTo (x,y,z) = unsafePerformIO $
        withProjectionPtr pjFrom $ \pjFrom' ->
        withProjectionPtr pjTo $ \pjTo' ->
        alloca $ \x' ->
        alloca $ \y' ->
        alloca $ \z' -> do
            poke x' $ convertDouble x
            poke y' $ convertDouble y
            poke z' $ convertDouble z
            err <- c_pjTransform  pjFrom' pjTo' 1 1 x' y' z'
            case err of
                0 -> do
                    x'' <- fmap convertDouble $ peek x'
                    y'' <- fmap convertDouble $ peek y'
                    z'' <- fmap convertDouble $ peek z'
                    return $ Just (x'',y'', z'')
                _ -> return Nothing

toRadian, fromRadian :: Double -> Double
toRadian = (/180) . (* pi)
{-# INLINE toRadian #-}
fromRadian = (/pi) . (*180)
{-# INLINE fromRadian #-}

isLatLong :: Projection -> Bool
isLatLong = toBool . unsafePerformIO . flip withProjectionPtr c_pjIsLatLong

toBool :: CInt -> Bool
toBool 0 = False
toBool _ = True

convertDouble :: (Real a, Fractional b) => a -> b
#ifdef __NHC__
convertDouble = realToFrac
#else
convertDouble = unsafeCoerce
#endif

