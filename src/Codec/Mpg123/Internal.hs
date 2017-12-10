{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, DeriveGeneric #-}
module Codec.Mpg123.Internal where

import Control.Monad (void)
import Foreign.C.Types
import Foreign.Ptr
-- import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Array (peekArray, peekArray0)
import System.Posix.Types
import GHC.Generics
-- import Foreign.Storable (Storable(..))
import qualified Control.Exception as E (bracket)
import Control.Monad.Catch
import qualified Language.C.Inline as C
import Control.Monad.IO.Class

import Codec.Mpg123.Internal.InlineC

C.context mpg123Ctx

C.include "<mpg123.h>"



-- | MP3 decoding to WAV : https://mpg123.de/api/feedseek_8c_source.shtml






-- | API : https://www.mpg123.de/api/group__mpg123__init.shtml


-- MPG123_EXPORT int 	mpg123_init (void)
mpg123init :: IO CInt
mpg123init = [C.exp| int{ mpg123_init() } |]

-- MPG123_EXPORT void 	mpg123_exit (void)
mpg123exit :: IO ()
mpg123exit = [C.exp| void{ mpg123_exit() }|]


-- | MPG123_EXPORT const char** mpg123_decoders (void )
-- mpg123decoders :: IO (Ptr (Ptr CChar))
mpg123decoder :: IO String
mpg123decoder = do
  s0 <- [C.exp| const char**{ mpg123_decoders( )}|] >>= peekArray 1
  peekCString $ head s0




-- | MPG123_EXPORT mpg123_handle* mpg123_new (const char *decoder, int *error)
-- | Create a handle with optional choice of decoder (named by a string, see mpg123_decoders() or mpg123_supported_decoders()). and optional retrieval of an error code to feed to mpg123_plain_strerror(). Optional means: Any of or both the parameters may be NULL.
-- Parameters
--     decoder	optional choice of decoder variant (NULL for default)
--     error	optional address to store error codes
-- Returns
--     Non-NULL pointer to fresh handle when successful. 
mpg123new :: Ptr CChar -> Ptr CInt -> IO (Ptr Mpg123_handle)
mpg123new decoder err = [C.exp| mpg123_handle*{ mpg123_new( $(const char* decoder), $(int* err))}|]

mpg123newDefault :: (MonadThrow m , MonadIO m) => m (Ptr Mpg123_handle)
mpg123newDefault = do
  mhptr <- liftIO $ snd <$> C.withPtr (\err ->[C.exp| mpg123_handle*{ mpg123_new( NULL, $(int* err))}|])
  handleErr mhptr
  

-- | MPG123_EXPORT void mpg123_delete (mpg123_handle *mh)
mpg123delete :: (MonadIO m, MonadThrow m) => Ptr Mpg123_handle -> m ()
mpg123delete mh = do
  liftIO [C.exp| void{ mpg123_delete( $(mpg123_handle* mh) )}|]
  void $ handleErr mh



-- | 'withHandle' is an exception-safe memory bracket
withHandle :: (Ptr Mpg123_handle -> IO a) -> IO a
withHandle = E.bracket mpg123newDefault mpg123delete




-- | MPG123_EXPORT int 	mpg123_param (mpg123_handle *mh, enum mpg123_parms type, long value, double fvalue)
-- | Set a specific parameter, for a specific mpg123_handle, using a parameter type key chosen from the mpg123_parms enumeration, to the specified value.
-- Parameters
--     mh	handle
--     type	parameter choice
--     value	integer value
--     fvalue	floating point value
-- Returns
--     MPG123_OK on success 
mpg123param ::
  (MonadThrow m, MonadIO m) =>
    Ptr Mpg123_handle -> Mpg123_parms -> CLong -> CDouble -> m (Ptr Mpg123_handle)
mpg123param mh ty val fval = do 
  void $ liftIO [C.exp| int{ mpg123_param($(mpg123_handle* mh), $(int ty'), $(long val), $(double fval))}|]
  handleErr mh
  where
  ty' = fromParms ty
  
-- | MPG123_EXPORT int 	mpg123_getparam (mpg123_handle *mh, enum mpg123_parms type, long *value, double *fvalue)
mpg123getParam :: Ptr Mpg123_handle -> Mpg123_parms -> Ptr CLong -> Ptr CDouble -> IO CInt
mpg123getParam mh ty val fval = [C.exp| int{ mpg123_getparam($(mpg123_handle* mh), $(int ty'), $(long* val), $(double* fval))}|] where
  ty' = fromParms ty
  
-- MPG123_EXPORT int 	mpg123_feature (const enum mpg123_feature_set key)



-- | MPG123_EXPORT int mpg123_open_feed (mpg123_handle* mh)
-- | Open a new bitstream and prepare for direct feeding This works together with mpg123_decode(); you are responsible for reading and feeding the input bitstream.
-- Parameters
--     mh	handle
-- Returns
--     MPG123_OK on success 
mpg123openFeed
  :: (MonadIO m, MonadThrow m) =>
     Ptr Mpg123_handle -> m (Ptr Mpg123_handle)
mpg123openFeed mh = do
  void $ liftIO [C.exp|int{ mpg123_open_feed( $(mpg123_handle* mh) )}|]
  handleErr mh




-- | MPG123_EXPORT off_t mpg123_feedseek (mpg123_handle* mh, off_t sampleoff, int whence, off_t* input_offset )
mpg123feedSeek :: Ptr Mpg123_handle -> COff -> CInt -> Ptr COff -> IO COff
mpg123feedSeek mh sampleoff whence inpoff = [C.exp| off_t{ mpg123_feedseek($(mpg123_handle* mh), $(off_t sampleoff), $(int whence), $(off_t* inpoff))}|]

-- | MPG123_EXPORT int mpg123_feed (mpg123_handle* mh, const unsigned char* in, size_t size )
-- | Feed data for a stream that has been opened with mpg123_open_feed(). It's give and take: You provide the bytestream, mpg123 gives you the decoded samples. 
-- Parameters
--     mh	handle
--     in	input buffer
--     size	number of input bytes
-- Returns
--     MPG123_OK or error/message code. 
mpg123feed
  :: (MonadIO m, MonadThrow m) =>
     Ptr Mpg123_handle -> Ptr CChar -> CSize -> m (Ptr Mpg123_handle)
mpg123feed mh inchr sz = do
  void $ liftIO [C.exp| int{ mpg123_feed( $(mpg123_handle* mh), $(char* inchr), $(size_t sz))}|]
  handleErr mh

-- | MPG123_EXPORT int mpg123_decode ( mpg123_handle* mh, const unsigned char* inmemory, size_t inmemsize, unsigned char* outmemory, size_t outmemsize, size_t* done)
-- | Decode MPEG Audio from inmemory to outmemory. This is very close to a drop-in replacement for old mpglib. When you give zero-sized output buffer the input will be parsed until decoded data is available. This enables you to get MPG123_NEW_FORMAT (and query it) without taking decoded data. Think of this function being the union of mpg123_read() and mpg123_feed() (which it actually is, sort of;-). You can actually always decide if you want those specialized functions in separate steps or one call this one here.
-- Parameters
--     mh	handle
--     inmemory	input buffer
--     inmemsize	number of input bytes
--     outmemory	output buffer
--     outmemsize	maximum number of output bytes
--     done	address to store the number of actually decoded bytes to
-- Returns
--     error/message code (watch out especially for MPG123_NEED_MORE)
mpg123decode :: (MonadThrow m, MonadIO m) =>
                                 Ptr Mpg123_handle
                                 -> Ptr CUChar
                                 -> CSize
                                 -> Ptr CUChar
                                 -> CSize
                                 -> Ptr CSize
                                 -> m (Ptr Mpg123_handle)
mpg123decode mh inmem inmemsz outmem outmemsz done = do 
  void $ liftIO [C.exp| int{ mpg123_decode( $(mpg123_handle* mh), $(unsigned char* inmem), $(size_t inmemsz), $(unsigned char* outmem), $(size_t outmemsz), $(size_t* done)) }|]
  handleErr mh


-- | MPG123_EXPORT int mpg123_decode_frame (mpg123_handle* mh, off_t* num, unsigned char** audio, size_t* bytes )
-- | Decode next MPEG frame to internal buffer or read a frame and return after setting a new format.
-- Parameters
--     mh	handle
--     num	current frame offset gets stored there
--     audio	This pointer is set to the internal buffer to read the decoded audio from.
--     bytes	number of output bytes ready in the buffer
-- Returns
--     MPG123_OK or error/message code
mpg123decodeFrame :: (MonadThrow m, MonadIO m) =>
                           Ptr Mpg123_handle
                           -> Ptr COff
                           -> Ptr (Ptr CUChar)
                           -> Ptr CSize
                           -> m (Ptr Mpg123_handle)
mpg123decodeFrame mh num audio bytes = do 
  void $ liftIO [C.exp| int{ mpg123_decode_frame( $(mpg123_handle* mh), $(off_t* num), $(unsigned char** audio), $(size_t* bytes))}|]
  handleErr mh

-- | MPG123_EXPORT int mpg123_getformat (mpg123_handle* mh, long* rate, int* channels, int* encoding )
-- | Get the current output format written to the addresses given. If the stream is freshly loaded, this will try to parse enough of it to give you the format to come. This clears the flag that would otherwise make the first decoding call return MPG123_NEW_FORMAT. 
-- Parameters
--     mh	handle
--     rate	sampling rate return address
--     channels	channel count return address
--     encoding	encoding return address
-- Returns
--     MPG123_OK on success 
mpg123getFormat ::
  (MonadThrow m, MonadIO m) =>
       Ptr Mpg123_handle -> Ptr CLong -> Ptr CInt -> Ptr CInt -> m (Ptr Mpg123_handle)
mpg123getFormat mh rate channels encoding = do 
  void $ liftIO [C.exp|int{ mpg123_getformat( $(mpg123_handle* mh), $(long* rate), $(int* channels), $(int* encoding))}|]
  handleErr mh




-- * Errors

-- https://mpg123.de/api/group__mpg123__error.shtml


-- | MPG123_EXPORT const char* mpg123_strerror (mpg123_handle* mh)
-- | Give string describing what error has occured in the context of handle mh. When a function operating on an mpg123 handle returns MPG123_ERR, you should check for the actual reason via char *errmsg = mpg123_strerror(mh) This function will catch mh == NULL and return the message for MPG123_BAD_HANDLE.
mpg123strError :: Ptr Mpg123_handle -> IO String
mpg123strError mh = [C.exp| const char*{ mpg123_strerror( $(mpg123_handle* mh))}|] >>= peekCString



-- | MPG123_EXPORT int mpg123_errcode ( mpg123_handle* mh)
-- | Return the current integer error code associated with the handle
mpg123errCode :: Ptr Mpg123_handle -> IO CInt
mpg123errCode mh = [C.exp| int{ mpg123_errcode( $(mpg123_handle* mh) )} |]





handleErr :: (MonadIO m, MonadThrow m) =>
          (Ptr Mpg123_handle)
       -> m (Ptr Mpg123_handle)
handleErr mhptr = do
  errstr <- liftIO $ mpg123strError mhptr
  ierr <- liftIO $ mpg123errCode mhptr
  case toEnum (fromIntegral ierr) of EOk -> pure mhptr
                                     EDone -> pure mhptr
                                     _ -> throwM $ Mpg123Exception errstr                                     
                                     

data Mpg123Exception = Mpg123Exception String deriving (Eq, Show, Generic)
instance Exception Mpg123Exception where




-- -- * Helpers

fromParms :: Mpg123_parms -> CInt
fromParms ty = CInt $ fromIntegral $ fromEnum (ty :: Mpg123_parms)





