{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module OSGeo.Proj4 (
    Projection
  , Projectable (transform)
  , createProjection
) where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe (unsafePerformIO)

data ProjectionCtx
data ProjectionPtr

data Projection = Projection (Ptr ProjectionPtr) (MVar ())

foreign import ccall "proj.h proj_context_create" c_pjContextCreate
  :: IO (Ptr ProjectionCtx)
        
foreign import ccall "proj.h proj_create_crs_to_crs" c_pjCreateCrsToCrs
  :: Ptr ProjectionCtx -> CString -> CString -> Ptr () -> IO (Ptr ProjectionPtr)

foreign import ccall "proj.h proj_trans_array" c_pjTransArray
  :: Ptr ProjectionPtr -> CInt -> CInt -> Ptr CDouble -> IO CInt

createProjection :: String -> String -> IO (Either String Projection)
createProjection from to = 
  withCString from $ \cFrom ->
  withCString to $ \cTo -> do
    ctx <- c_pjContextCreate
    if ctx == nullPtr
      then pure $ Left "createProjection: could not initialize context"
      else do
        ptr <- c_pjCreateCrsToCrs ctx cFrom cTo nullPtr
        if ptr == nullPtr
            then pure $ Left $ "createProjection: could not initialize projection " <> "'" <> from <> " -> " <> to  <> "'"
            else do
              mutex <- MVar.newMVar ()
              pure $ Right $ Projection ptr mutex
    
class Projectable a where
  transform :: Projection -> a -> Maybe a

pjFwd :: CInt
pjFwd = 1

instance Projectable (Double, Double) where
  {-# NOINLINE transform #-}
  transform (Projection ptr mutex) (x, y) = unsafePerformIO $ MVar.withMVar mutex $ \_ ->
    withArray [CDouble x, CDouble y, 0, 0] $ \c -> do
      err <- c_pjTransArray ptr pjFwd 1 c

      case err of
        0 -> do
          !x' <- peekElemOff c 0
          !y' <- peekElemOff c 1
          pure $! Just $! (realToFrac x', realToFrac y')

        _ -> pure Nothing

instance Projectable (Double, Double, Double) where
  {-# NOINLINE transform #-}
  transform (Projection ptr mutex) (x, y, z) = unsafePerformIO $ MVar.withMVar mutex $ \_ ->
    withArray [CDouble x, CDouble y, CDouble z, 0] $ \c -> do
      err <- c_pjTransArray ptr pjFwd 1 c

      case err of
        0 -> do
          !x' <- peekElemOff c 0
          !y' <- peekElemOff c 1
          !z' <- peekElemOff c 2
          pure $! Just $! (realToFrac x', realToFrac y', realToFrac z')

        _ -> pure Nothing
