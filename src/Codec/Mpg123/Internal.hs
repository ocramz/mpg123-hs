{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, DeriveGeneric #-}
{-|
Module      : Codec.Mpg123.Internal
Description : Low- and mid-level mpg123 bindings
Copyright   : (c) Marco Zocca, 2017
License     : BSD2
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX

This module contains the lowest-level inline-C bindings and some safety rails for memory- and exception-management around these.
-}
module Codec.Mpg123.Internal (
    decode

  , withMpg123
  -- * Byte input
  , readBufferedFile
  , readWholeFile
  , mpg123openFeed
  -- , mpg123feed
  -- , mpg123feedSeek
  -- * Decoding
  , mpg123decode
  -- *
  , mpg123getFormat
  -- , mpg123decodeFrame
  -- * Configuration
  , mpg123paramInt
  -- * Error output
  , mpg123strError
  , mpg123errCode
  -- * Utilities
  , mpg123decoders  
  -- ** File I/O
  , readChunkedHdl
  , writeChunkedHdl
  , readWriteHdl
  -- ** Exception handling
  , handleErr
  -- ** Storable-related
  , withPtr2
  , withPtr3
  -- ** Buffer size
  , bufSizeInDefault
  ,) where

import Data.List

import qualified Control.Exception as E (Exception(..), bracket)

import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(..), throwM)

import System.Posix.Types
import System.IO -- (withBinaryFile, IOMode(..), hFileSize)

import GHC.Generics
import GHC.IO.Buffer
import GHC.IO.BufferedIO
import GHC.IO.Handle

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as LB
-- import qualified Data.ByteString.Char8 as B8 (copy, useAsCString)
import qualified Data.ByteString.Lazy.Char8 as LB8 (hGetContents, hGet, hPut, toChunks, take)
-- import qualified Data.ByteString.Streaming as BS --  (ByteString, stdout, hGetContentsN)

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS (Vector, unsafeWith, unsafeFromForeignPtr0, fromList)

import Data.Bits
import Data.Int (Int8)  -- CChar
import Data.Word (Word8)  -- CUChar

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, mallocForeignPtrArray)
import Foreign.Storable (Storable(..), pokeByteOff)
import Foreign.Marshal.Array (allocaArray, allocaArray0, peekArray, peekArray0)
import Foreign.Marshal.Alloc (allocaBytes)

import qualified Language.C.Inline as C
import Control.Monad.IO.Class

import Codec.Mpg123.Internal.InlineC
import Data.Utils (wi8, iw8)





C.context mpg123Ctx

C.include "<mpg123.h>"

-- * File input


-- | Read a binary file
readBinaryFile :: FilePath -> (Handle -> IO r) -> IO r
readBinaryFile fpath = withBinaryFile fpath ReadMode

-- | Read a binary file with knowledge of the file size
readBinaryFileWSize ::
     FilePath                    -- ^ Input file path
  -> (Integer -> Handle -> IO r) -- ^ (File size, file handle)
  -> IO r
readBinaryFileWSize fpath f = withBinaryFile fpath ReadMode g
  where
    g h = do 
      fs <- hFileSize h
      f fs h

-- | Write a binary file
writeBinaryFile :: FilePath -> (Handle -> IO r) -> IO r
writeBinaryFile fpath = withBinaryFile fpath WriteMode






-- | Open a binary file in read-only mode, load its contents as a lazy bytestring, split this into a list of /strict/ bytestrings and treat each as a raw memory array. 
readBufferedFile
  :: FilePath
  -> (CSize -> Ptr CUChar -> IO a)  -- ^ (chunk length, pointer to byte)
  -> IO [a]
readBufferedFile fpath f = readBinaryFile fpath (helper f)
  where
  helper :: (CSize -> Ptr CUChar -> IO a) -> Handle -> IO [a]
  helper f hdl = do
    lbs <- LB8.hGetContents hdl
    let bs = LB8.toChunks lbs
    sequenceA $ withLen f <$> bs
      where
        withLen g b = do
          let lenB = B.length b
          debug ["Input buffer:", show lenB, "bytes"]          
          B.useAsCString b $ \p -> g (fi lenB) (castPtr p)
        -- withLen g b = useAsCUString b $ g (fi (B.length b))
        fi = C.CSize . fromIntegral

bufSizeInDefault :: Integer
bufSizeInDefault = 2^14



-- | Open a binary file in read-only mode, load its contents as a lazy bytestring, copy this to a /strict/ bytestring and treat it as a raw memory array. 
readWholeFile
  :: FilePath
  -> (CSize -> Ptr CUChar -> IO a)  -- ^ (chunk length, pointer to byte)
  -> IO a
readWholeFile fpath f = readBinaryFile fpath (helper f) where
  helper f hdl = do
     lbs <- LB8.hGetContents hdl
     let
         bs = LB.toStrict lbs
         lenB = B.length bs       
     if lenB == 0
       then throwM $ Mpg123Exception "Zero-length input data"
       else do
         debug ["Input data:", show lenB, "bytes"]
         withLen f bs
     where
         withLen g b = B.useAsCString b $ \p -> g (fi (B.length b)) (castPtr p)
         fi = C.CSize . fromIntegral
  
-- useAsCUString :: BI.ByteString -> (Ptr CUChar -> IO a) -> IO a
-- useAsCUString (BI.PS fp o l) action =
--  allocaBytes (l + 1) $ \buf ->
--    withForeignPtr fp $ \p -> do
--      BI.memcpy buf (p `plusPtr` o) (fromIntegral l)
--      pokeByteOff buf l (0 :: Int8)
--      action (castPtr buf)

readWriteHdl :: FilePath -> FilePath -> IO ()
readWriteHdl  fin fout = void $ readBinaryFile fin $ \fhin ->
  writeBinaryFile fout $ \fhout ->
    readChunkedHdl fhin (writeChunkedHdl fhout)

readChunkedHdl :: Handle -> (BI.ByteString -> IO b) -> IO [b]
readChunkedHdl hdl f = do
  lbs <- LB8.hGetContents hdl
  let bs = LB8.toChunks lbs
  traverse f bs

writeChunkedHdl :: Handle -> BI.ByteString -> IO ()
writeChunkedHdl hdl ibs = do
  LB8.hPut hdl $ LB.fromStrict ibs
  

-- | High-level wrapper around 'mpg123decode'
decode ::
     -- Verbosity        -- ^ Verbosity level
    FilePath   -- ^ Input file path
  -> FilePath   -- ^ Output file path
  -> CSize      -- ^ Output buffer size
  -> IO ()
decode fin fout outmemsz = void $ withMpg123 $ \mh ->
  readBufferedFile fin $ \inmemsz inmem ->
  -- readWholeFile fin $ \inmemsz inmem ->
    writeBinaryFile fout $ \hdlOut -> do
        mpg123paramInt mh (verbose Verbose)
        debug ["Set verbosity"]
        mpg123openFeed mh
        debug ["Open feed"]
        -- outmem <- maybe B.empty id <$> mpg123decode mh inmem inmemsz outmemsz
        (outmem, ierr) <- mpg123decode1 mh inmem inmemsz outmemsz
        debug ["Decoded data:", show (B.length outmem), "bytes"]
        debug ["mpg123decode() returned:", show (ierr)]        
        writeChunkedHdl hdlOut outmem


  
     




-- | MP3 decoding to WAV : https://mpg123.de/api/feedseek_8c_source.shtml



-- | @MPG123_EXPORT int mpg123_decode ( mpg123_handle* mh, const unsigned char* inmemory, size_t inmemsize, unsigned char* outmemory, size_t outmemsize, size_t* done)@
--
-- Decode MPEG Audio from inmemory to outmemory. This is very close to a drop-in replacement for old @mpglib@. When you give zero-sized output buffer the input will be parsed until decoded data is available. This enables you to get @MPG123_NEW_FORMAT@ (and query it) without taking decoded data. Think of this function being the union of @mpg123_read()@ and @mpg123_feed()@ (which it actually is, sort of;-). You can actually always decide if you want those specialized functions in separate steps or one call this one here.
--
-- Parameters
--
-- *    mh	handle
-- *    inmemory	input buffer
-- *    inmemsize	number of input bytes
-- *    outmemory	output buffer
-- *    outmemsize	maximum number of output bytes
-- *    done	address to store the number of actually decoded bytes to
--
-- Returns
--
-- *    error/message code (watch out especially for MPG123_NEED_MORE)
mpg123decode ::
     Ptr Mpg123_handle   -- ^ Handle 
  -> Ptr CUChar          -- ^ Input memory buffer
  -> CSize                 -- ^ Size of input memory buffer (bytes)
  -> CSize               -- ^ Size of output memory buffer (bytes)
  -> IO (Maybe BI.ByteString)
mpg123decode mh inmem inmemsz outmemsz = do 
  (sz, vec) <- C.withPtr $ \done -> do 
      fpo <- BI.mallocByteString (fromIntegral outmemsz)
      withForeignPtr fpo $ \outmem -> do
        void $ mpg123getFormat mh        
        void $ mpg123decode' mh inmem inmemsz outmemsz done outmem
        void $ handleErr mh
        B.packCString $ castPtr outmem
  if (fromIntegral sz :: Int) > 0
    then return $ Just vec
    else return Nothing  

 
mpg123decode1 :: Ptr Mpg123_handle
     -> Ptr CUChar -> CSize -> CSize -> IO (BI.ByteString, CInt)
mpg123decode1 mh inmem inmemsz outmemsz = do
  (a, b, c) <- mpg123getFormat mh
  debug ["Format : ", show a, show b, show c]  
  (_, vecIerr) <- C.withPtr $ \done -> do 
      fpo <- BI.mallocByteString (fromIntegral outmemsz)
      withForeignPtr fpo $ \outmem -> do
        ierr1 <- mpg123decode' mh inmem inmemsz outmemsz done outmem
        debug ["First ierr:", show ierr1]
        -- void $ handleErr mh
        void $ mpg123getFormat mh
        ierr <- mpg123decodeBuf' mh outmemsz done outmem
        debug ["Second ierr:", show ierr]        
        v <- B.packCString $ castPtr outmem
        return (v, ierr)
  return vecIerr


debug :: [String] -> IO ()
debug = putStrLn . unwords
  

mpg123decode' ::
  Ptr Mpg123_handle -> Ptr CUChar -> CSize -> CSize -> Ptr CSize -> Ptr CUChar -> IO CInt
mpg123decode' mh inmem inmemsz outmemsz done outmem =
  [C.exp| int{ mpg123_decode( $(mpg123_handle* mh), $(unsigned char* inmem), $(size_t inmemsz), $(unsigned char* outmem), $(size_t outmemsz), $(size_t* done)) }|]


-- | Like mpg123decode', but it uses buffered input data
mpg123decodeBuf' :: Ptr Mpg123_handle -> CSize -> Ptr CSize -> Ptr CUChar -> IO CInt
mpg123decodeBuf' mh outmemsz done outmem =
  [C.exp| int{ mpg123_decode( $(mpg123_handle* mh), NULL, 0, $(unsigned char* outmem), $(size_t outmemsz), $(size_t* done)) }|]











-- | @MPG123_EXPORT int mpg123_param (mpg123_handle *mh, enum mpg123_parms type, long value, double fvalue)@
--
-- Set a specific parameter, for a specific @mpg123_handle@, using a parameter type key chosen from the @mpg123_parms@ enumeration, to the specified value.
--
-- Parameters
--
-- *     mh	handle
-- *     type	parameter choice
-- *     value	integer value
-- *     fvalue	floating point value
--
-- Returns
--
-- *    MPG123_OK on success 
mpg123paramInt :: Ptr Mpg123_handle -> Param -> IO ()
mpg123paramInt mh p = do
  void [C.exp| int{ mpg123_param($(mpg123_handle* mh), $(int ty'), $(long val'), 0)}|]
  void $ handleErr mh
  where
    (ty', val') = fromParms p

fromParms :: Param -> (CInt, CLong)
fromParms (IntParam pty v) = (fromParmsTy pty, fromIntegral v)

fromParmsTy :: Mpg123_parms -> CInt
fromParmsTy ty = CInt $ fromIntegral $ fromEnum (ty :: Mpg123_parms)
  
--  @MPG123_EXPORT int mpg123_getparam (mpg123_handle *mh, enum mpg123_parms type, long *value, double *fvalue)@
-- mpg123getParam' :: Ptr Mpg123_handle -> Mpg123_parms -> Ptr CLong -> Ptr CDouble -> IO CInt
-- mpg123getParam' mh ty val fval = [C.exp| int{ mpg123_getparam($(mpg123_handle* mh), $(int ty'), $(long* val), $(double* fval))}|] where
--   ty' = fromParmsTy ty
  
-- MPG123_EXPORT int mpg123_feature (const enum mpg123_feature_set key)



-- | @MPG123_EXPORT int mpg123_open_feed (mpg123_handle* mh)@
--
-- Open a new bitstream and prepare for direct feeding. This works together with @mpg123_decode()@; you are responsible for reading and feeding the input bitstream.
--
-- Parameters
--
-- *    mh	handle
--
-- Returns
--
-- *    MPG123_OK on success 
mpg123openFeed
  :: (MonadIO m, MonadThrow m) =>
     Ptr Mpg123_handle -> m ()
mpg123openFeed mh = do
  void $ liftIO [C.exp|int{ mpg123_open_feed( $(mpg123_handle* mh) )}|]
  void $ handleErr mh




-- | @MPG123_EXPORT off_t mpg123_feedseek (mpg123_handle* mh, off_t sampleoff, int whence, off_t* input_offset )@
mpg123feedSeek :: Ptr Mpg123_handle -> COff -> CInt -> Ptr COff -> IO COff
mpg123feedSeek mh sampleoff whence inpoff = [C.exp| off_t{ mpg123_feedseek($(mpg123_handle* mh), $(off_t sampleoff), $(int whence), $(off_t* inpoff))}|]



-- | @MPG123_EXPORT int mpg123_feed (mpg123_handle* mh, const unsigned char* in, size_t size)@
--
-- Feed data for a stream that has been opened with @mpg123_open_feed()@. It's give and take: You provide the bytestream, @mpg123@ gives you the decoded samples.
--
-- Parameters
--
-- *     mh	handle
-- *   in	input buffer
-- *   size	number of input bytes
--
-- Returns
--
-- *    MPG123_OK or error/message code. 
mpg123feed
  :: (MonadIO m, MonadThrow m) =>
     Ptr Mpg123_handle -> Ptr CUChar -> CSize -> m (Ptr Mpg123_handle)
mpg123feed mh inchr sz = do
  void $ liftIO [C.exp| int{ mpg123_feed( $(mpg123_handle* mh), $(const unsigned char* inchr), $(size_t sz))}|]
  handleErr mh






-- | @MPG123_EXPORT int mpg123_decode_frame (mpg123_handle* mh, off_t* num, unsigned char** audio, size_t* bytes )@
--
-- Decode next MPEG frame to internal buffer or read a frame and return after setting a new format.
--
-- Parameters
--
-- *     mh	handle
-- *    num	current frame offset gets stored there
-- *    audio	This pointer is set to the internal buffer to read the decoded audio from.
-- *    bytes	number of output bytes ready in the buffer
--
-- Returns
--
-- *    MPG123_OK or error/message code
mpg123decodeFrame :: Ptr Mpg123_handle -> IO (Maybe BI.ByteString)
mpg123decodeFrame mh = do
  (_, audio, nbytes, _) <- withPtr3 (mpg123decodeFrame' mh)
  void $ handleErr mh
  bs <- B.packCString $ castPtr audio
  if nbytes > 0
    then return $ Just bs -- (num, bs)
    else return Nothing
  
  
mpg123decodeFrame' :: Ptr Mpg123_handle -> Ptr COff -> Ptr (Ptr CUChar) -> Ptr CSize -> IO CInt
mpg123decodeFrame' mh num audio bytes =
  [C.exp| int{ mpg123_decode_frame( $(mpg123_handle* mh), $(off_t* num), $(unsigned char** audio), $(size_t* bytes))}|]

-- | @MPG123_EXPORT int mpg123_getformat (mpg123_handle* mh, long* rate, int* channels, int* encoding )@
-- Get the current output format written to the addresses given. If the stream is freshly loaded, this will try to parse enough of it to give you the format to come. This clears the flag that would otherwise make the first decoding call return @MPG123_NEW_FORMAT@.
--
-- Parameters
--
-- *    mh	handle
-- *    rate	sampling rate return address
-- *    channels	channel count return address
-- *    encoding	encoding return address
--
-- Returns
--
-- *    MPG123_OK on success 
mpg123getFormat :: (MonadThrow m, MonadIO m) =>
                         Ptr Mpg123_handle -> m (CLong, CInt, CInt)
mpg123getFormat mh = do 
   (rt, ch, enc, _) <- liftIO $ withPtr3 $ \rate channels encoding -> 
     [C.exp|int{ mpg123_getformat( $(mpg123_handle* mh), $(long* rate), $(int* channels), $(int* encoding))}|]
   -- void $ handleErr mh
   return (rt, ch, enc)







-- | *** FIXME *** doesn't compile due to returned `struct`
-- @MPG123_EXPORT int mpg123_info(mpg123_handle* mh, struct mpg123_frameinfo* mi )@
-- Get frame information about the MPEG audio bitstream and store it in a mpg123_frameinfo structure.
-- -- Parameters
-- --     mh	handle
-- --     mi	address of existing frameinfo structure to write to
-- -- Returns
-- --     MPG123_OK on success
-- -- mpg123info
-- --   :: (MonadIO m, MonadThrow m) => Ptr Mpg123_handle -> m MpgFrameInfo
-- mpg123info mh = do 
--   (finfo, _) <- liftIO $ C.withPtr $ \mi -> [C.exp| int{ mpg123_info( $(mpg123_handle* mh), $(struct mpg123_frameinfo* mi) ) }|]
--   void $ handleErr mh
--   return finfo



-- | API : https://www.mpg123.de/api/group__mpg123__init.shtml


-- MPG123_EXPORT int 	mpg123_init (void)
mpg123init :: IO CInt
mpg123init = [C.exp| int{ mpg123_init() } |]

-- MPG123_EXPORT void 	mpg123_exit (void)
mpg123exit :: IO ()
mpg123exit = [C.exp| void{ mpg123_exit() }|]


-- | @MPG123_EXPORT const char** mpg123_decoders (void )@
--
-- @mpg123decoders n@ : Output a list of @n@ strings (usually 4) with a text description of the MP3 decoders used by @libmpg123@ 
mpg123decoders :: Int -> IO [String]
mpg123decoders n = do
  s <- mpg123decoderString' >>= peekArray n
  traverse peekCString s  

mpg123decoderString' :: IO (Ptr (Ptr CChar))
mpg123decoderString' = [C.exp| const char**{ mpg123_decoders( )}|]



-- | @MPG123_EXPORT mpg123_handle* mpg123_new (const char *decoder, int *error)@
--
-- Create a handle with optional choice of decoder (named by a string, see @mpg123_decoders()@ or @mpg123_supported_decoders()@). and optional retrieval of an error code to feed to @mpg123_plain_strerror()@. Optional means: Any of or both the parameters may be NULL.
--
-- Parameters
--     decoder	optional choice of decoder variant (NULL for default)
--     error	optional address to store error codes
-- Returns
--     Non-NULL pointer to fresh handle when successful. 
mpg123new ::
     Ptr CChar
  -> Ptr CInt
  -> IO (Ptr Mpg123_handle)
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



-- | 'withMpg123' is an exception-safe memory 'bracket' that allocates a 'Mpg123_handle' object and disposes of it at the end of the bracketed function supplied as a parameter. Memory is reclaimed also if the bracketed function throws an exception.
withMpg123 :: (Ptr Mpg123_handle -> IO a) -> IO a
withMpg123 = E.bracket finit mpg123delete where
  finit = do
    void mpg123init
    mpg123newDefault


-- | MPG123_EXPORT int mpg123_close (mpg123_handle* mh) 	
-- Closes the source, if libmpg123 opened it.
-- Parameters
--     mh	handle
-- Returns
-- MPG123_OK on success
mpg123close :: (MonadIO m, MonadThrow m) => Ptr Mpg123_handle -> m ()
mpg123close mh = do
  void $ liftIO [C.exp|int{ mpg123_close( $(mpg123_handle* mh) )}|]
  void $ handleErr mh






-- * Errors

-- https://mpg123.de/api/group__mpg123__error.shtml


-- | @MPG123_EXPORT const char* mpg123_strerror (mpg123_handle* mh)@
--
-- Give string describing what error has occured in the context of handle mh. When a function operating on an mpg123 handle returns @MPG123_ERR@, you should check for the actual reason via @char *errmsg = mpg123_strerror(mh)@. This function will catch @mh == NULL@ and return the message for @MPG123_BAD_HANDLE@.
mpg123strError :: Ptr Mpg123_handle -> IO String
mpg123strError mh = [C.exp| const char*{ mpg123_strerror( $(mpg123_handle* mh))}|] >>= peekCString



-- | @MPG123_EXPORT int mpg123_errcode ( mpg123_handle* mh)@
--
-- Return the current integer error code associated with the handle
mpg123errCode :: Ptr Mpg123_handle -> IO CInt
mpg123errCode mh = [C.exp| int{ mpg123_errcode( $(mpg123_handle* mh) )} |]



-- handleErr' mhptr = do
--   errstr <- liftIO $ mpg123strError mhptr  
--   ierr <- liftIO $ mpg123errCode mhptr
--   case toEnum (fromIntegral ierr) of 


-- | Decode integer @libmpg123@ error codes into Haskell exception values
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
instance E.Exception Mpg123Exception where




-- * Helpers




  

  

-- -- | This version of allocVS uses VS.unsafeFromForeignPtr0 which outputs a VS.Vector copy of the dereferenced memory in O(1)
-- allocVS :: (Storable a, Integral n) =>
--      n -> (Ptr a -> IO t) -> IO (VS.Vector a)
-- allocVS n' mf = do
--   let n = fromIntegral n'
--   fp <- mallocForeignPtrArray n
--   void $ withForeignPtr fp mf
--   return $ VS.unsafeFromForeignPtr0 fp n

-- allocVS :: Storable a => Int -> (Ptr a1 -> IO a) -> IO (VS.Vector a)
-- allocVS n mf = allocaArray n $ \p -> do
--   ierr <- mf p
--   arr <- peekArray n p
--   return $ VS.fromList arr


withPtr2 :: (Storable t1, Storable t2) =>
                  (Ptr t2 -> Ptr t1 -> IO t) -> IO (t2, t1, t)
withPtr2 m = do 
   (a, (b, c)) <- C.withPtr $ \p1 ->
                    C.withPtr $ \p2 -> m p1 p2
   return (a, b, c)

withPtr3 :: (Storable t1, Storable t2, Storable t3) =>
                  (Ptr t3 -> Ptr t2 -> Ptr t1 -> IO t) -> IO (t3, t2, t1, t)
withPtr3 m = do 
  (a, (b, (c, d))) <- C.withPtr $ \p1 ->
                        C.withPtr $ \p2 ->
                          C.withPtr $ \p3 -> m p1 p2 p3
  return (a, b, c, d)
