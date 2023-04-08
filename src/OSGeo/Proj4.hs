{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module OSGeo.Proj4 (
    Projection
  , Projectable (transform)
  , projection
) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)

data ProjectionCtx
data ProjectionPtr

newtype Projection = Projection {unProjection :: ForeignPtr ProjectionPtr}

withProjectionPtr :: Projection -> (Ptr ProjectionPtr -> IO a) -> IO a
withProjectionPtr = withForeignPtr . unProjection

foreign import ccall "proj.h proj_context_create" c_pjContextCreate
  :: IO (Ptr ProjectionCtx)
        
foreign import ccall "proj.h proj_create_crs_to_crs" c_pjCreateCrsToCrs
  :: Ptr ProjectionCtx -> CString -> CString -> Ptr () -> IO (Ptr ProjectionPtr)

foreign import ccall "proj.h proj_trans_array" c_pjTransArray
  :: Ptr ProjectionPtr -> CInt -> CInt -> Ptr CDouble -> IO CInt

foreign import ccall "proj.h &proj_destroy" c_pjDestroy
  :: FunPtr (Ptr ProjectionPtr -> IO ())

projection :: String -> String -> Either String Projection
projection from to = unsafePerformIO $
  withCString from $ \cFrom ->
  withCString to $ \cTo -> do
    ctx <- c_pjContextCreate
    ptr <- c_pjCreateCrsToCrs ctx cFrom cTo nullPtr

    if ptr == nullPtr
        then return $ Left $ "projection: could not initialize projection " ++
                             "'" <> from <> " -> " <> to  <> "'"
        else fmap (Right . Projection) $ newForeignPtr c_pjDestroy ptr
    
class Projectable a where
  transform :: Projection -> a -> Maybe a

pjFwd :: CInt
pjFwd = 1

instance Projectable (Double, Double) where
  transform p (x, y) = unsafePerformIO $
    withProjectionPtr p $ \p' ->
    withArray [CDouble x, CDouble y, 0, 0] $ \c -> do
      err <- c_pjTransArray p' pjFwd 1 c

      case err of
        0 -> do
          x' <- peekElemOff c 0
          y' <- peekElemOff c 1
          pure $ Just (realToFrac x', realToFrac y')

        _ -> pure Nothing

instance Projectable (Double, Double, Double) where
  transform p (x, y, z) = unsafePerformIO $
    withProjectionPtr p $ \p' ->
    withArray [CDouble x, CDouble y, CDouble z, 0] $ \c -> do
      err <- c_pjTransArray p' pjFwd 1 c

      case err of
        0 -> do
          x' <- peekElemOff c 0
          y' <- peekElemOff c 1
          z' <- peekElemOff c 2
          pure $ Just (realToFrac x', realToFrac y', realToFrac z')

        _ -> pure Nothing
