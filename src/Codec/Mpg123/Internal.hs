{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Codec.Mpg123.Internal where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import System.Posix.Types

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as Ctx
import qualified Language.C.Types as C

import qualified Data.Map as M

C.include "<mpg123.h>"

-- | API : https://www.mpg123.de/api/group__mpg123__init.shtml


-- MPG123_EXPORT int 	mpg123_init (void)
mpg123init :: IO CInt
mpg123init = [C.exp| int{ mpg123_init() } |]

-- MPG123_EXPORT void 	mpg123_exit (void)
mpg123exit :: IO ()
mpg123exit = [C.exp| void{ mpg123_exit() }|]

-- MPG123_EXPORT mpg123_handle * 	mpg123_new (const char *decoder, int *error)
-- MPG123_EXPORT void 	mpg123_delete (mpg123_handle *mh)
-- MPG123_EXPORT int 	mpg123_param (mpg123_handle *mh, enum mpg123_parms type, long value, double fvalue)
-- MPG123_EXPORT int 	mpg123_getparam (mpg123_handle *mh, enum mpg123_parms type, long *value, double *fvalue)
-- MPG123_EXPORT int 	mpg123_feature (const enum mpg123_feature_set key)
