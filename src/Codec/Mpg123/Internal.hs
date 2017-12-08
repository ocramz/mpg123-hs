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
mpg123param :: Ptr Mpg123_handle -> Mpg123_parms -> CLong -> CDouble -> IO CInt
mpg123param mh ty val fval = [C.exp| int{ mpg123_param($(mpg123_handle* mh), $(int ty'), $(long val), $(double fval))}|] where
  ty' = fromParms ty
  
-- MPG123_EXPORT int 	mpg123_getparam (mpg123_handle *mh, enum mpg123_parms type, long *value, double *fvalue)
mpg123getParam :: Ptr Mpg123_handle -> Mpg123_parms -> Ptr CLong -> Ptr CDouble -> IO CInt
mpg123getParam mh ty val fval = [C.exp| int{ mpg123_param($(mpg123_handle* mh), $(int ty'), $(long* val), $(double* fval))}|] where
  ty' = fromParms ty
  
-- MPG123_EXPORT int 	mpg123_feature (const enum mpg123_feature_set key)



-- MPG123_EXPORT off_t mpg123_feedseek (mpg123_handle* mh, off_t sampleoff, int whence, off_t* input_offset )
mpg123feedSeek :: Ptr Mpg123_handle -> COff -> CInt -> Ptr COff -> IO COff
mpg123feedSeek mh sampleoff whence inpoff = [C.exp| off_t{ mpg123_feedseek($(mpg123_handle* mh), $(off_t sampleoff), $(int whence), $(off_t* inpoff))}|]

-- | Feed data for a stream that has been opened with mpg123_open_feed(). It's give and take: You provide the bytestream, mpg123 gives you the decoded samples. 
-- MPG123_EXPORT int mpg123_feed (mpg123_handle* mh, const unsigned char* in, size_t size )
mpg123feed :: Ptr Mpg123_handle -> Ptr CChar -> CSize -> IO CInt
mpg123feed mh inchr sz = [C.exp| int{ mpg123_feed( $(mpg123_handle* mh), $(char* inchr), $(size_t sz))}|]

-- | Decode MPEG Audio from inmemory to outmemory. This is very close to a drop-in replacement for old mpglib. When you give zero-sized output buffer the input will be parsed until decoded data is available. This enables you to get MPG123_NEW_FORMAT (and query it) without taking decoded data. Think of this function being the union of mpg123_read() and mpg123_feed() (which it actually is, sort of;-). You can actually always decide if you want those specialized functions in separate steps or one call this one here.
-- MPG123_EXPORT int mpg123_decode ( mpg123_handle* mh, const unsigned char* inmemory, size_t inmemsize, unsigned char* outmemory, size_t outmemsize, size_t* don

-- -- * Helpers

fromParms :: Mpg123_parms -> CInt
fromParms ty = CInt $ fromIntegral $ fromEnum (ty :: Mpg123_parms)





