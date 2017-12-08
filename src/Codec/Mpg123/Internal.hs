{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Codec.Mpg123.Internal where

import Codec.Mpg123.Internal.InlineC

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import System.Posix.Types

import           Foreign.Storable (Storable(..))


import qualified Language.C.Inline as C

C.context mpg123Ctx

C.include "<mpg123.h>"



-- | API : https://www.mpg123.de/api/group__mpg123__init.shtml


-- MPG123_EXPORT int 	mpg123_init (void)
mpg123init :: IO CInt
mpg123init = [C.exp| int{ mpg123_init() } |]

-- MPG123_EXPORT void 	mpg123_exit (void)
mpg123exit :: IO ()
mpg123exit = [C.exp| void{ mpg123_exit() }|]

-- MPG123_EXPORT mpg123_handle * 	mpg123_new (const char *decoder, int *error)
mpg123new :: Ptr CChar -> Ptr CInt -> IO (Ptr Mpg123_handle)
mpg123new decoder err = [C.exp| mpg123_handle*{ mpg123_new( $(char* decoder), $(int* err))}|]

-- MPG123_EXPORT void 	mpg123_delete (mpg123_handle *mh)
mpg123delete :: Ptr Mpg123_handle -> IO ()
mpg123delete mh = [C.exp| void{ mpg123_delete( $(mpg123_handle* mh ) )}|]

-- MPG123_EXPORT int 	mpg123_param (mpg123_handle *mh, enum mpg123_parms type, long value, double fvalue)
mpg123param :: Ptr Mpg123_handle -> CInt -> CLong -> CDouble -> IO CInt
mpg123param mh ty val fval = [C.exp| int{ mpg123_param($(mpg123_handle* mh), $(int ty), $(long val), $(double fval))}|]
-- MPG123_EXPORT int 	mpg123_getparam (mpg123_handle *mh, enum mpg123_parms type, long *value, double *fvalue)
-- MPG123_EXPORT int 	mpg123_feature (const enum mpg123_feature_set key)





