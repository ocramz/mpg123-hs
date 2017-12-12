{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, DeriveGeneric #-}
module Codec.Mpg123.Internal.InlineC where

import Data.Monoid
import Foreign.Storable
import System.Posix.Types
import GHC.Generics
import Language.C.Inline.Context (ctxTypesTable, baseCtx, funCtx, vecCtx, bsCtx)
import qualified Language.C.Types as C
import qualified Language.C.Inline as C
import Data.Bits
import Control.Exception
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..), throwM, catch)
import qualified Language.Haskell.TH as TH
import qualified Data.Map as M


C.include "<mpg123.h>"

-- | API : https://www.mpg123.de/api/group__mpg123__init.shtml

mpg123Ctx :: C.Context
mpg123Ctx = baseCtx <> funCtx <> vecCtx <> ctx where
  ctx = mempty { ctxTypesTable = mpg123TypesTable}




-- * Paramters

-- enum mpg123_parms
-- MPG123_VERBOSE = 0, set verbosity value for enabling messages to stderr, >= 0 makes sense (integer) 
-- MPG123_FLAGS, set all flags, p.ex val = MPG123_GAPLESS|MPG123_MONO_MIX (integer)
-- MPG123_ADD_FLAGS, add some flags (integer)
-- MPG123_FORCE_RATE, when value > 0, force output rate to that value (integer) 
-- MPG123_DOWN_SAMPLE, 0=native rate, 1=half rate, 2=quarter rate (integer)
-- MPG123_RVA, one of the RVA choices above (integer) 
-- MPG123_DOWNSPEED, play a frame N times (integer) 
-- MPG123_UPSPEED, play every Nth frame (integer) 
-- MPG123_START_FRAME, start with this frame (skip frames before that, integer)
-- MPG123_DECODE_FRAMES, decode only this number of frames (integer)
-- MPG123_ICY_INTERVAL, stream contains ICY metadata with this interval (integer) 
-- MPG123_OUTSCALE, the scale for output samples (amplitude - integer or float according to mpg123 output format, normally integer) 
-- MPG123_TIMEOUT, timeout for reading from a stream (not supported on win32, integer) 
-- MPG123_REMOVE_FLAGS, remove some flags (inverse of MPG123_ADD_FLAGS, integer) 
-- MPG123_RESYNC_LIMIT, Try resync on frame parsing for that many bytes or until end of stream (<0 ... integer). This can enlarge the limit for skipping junk on beginning, too (but not reduce it). 
-- MPG123_INDEX_SIZE Set the frame index size (if supported). Values <0 mean that the index is allowed to grow dynamically in these steps (in positive direction, of course) -- Use this when you really want a full index with every individual frame. 
-- MPG123_PREFRAMES Decode/ignore that many frames in advance for layer 3. This is needed to fill bit reservoir after seeking, for example (but also at least one frame in advance is needed to have all "normal" data for layer 3). Give a positive integer value, please.*/
-- MPG123_FEEDPOOL For feeder mode, keep that many buffers in a pool to avoid frequent malloc/free. The pool is allocated on mpg123_open_feed(). If you change this parameter afterwards, you can trigger growth and shrinkage during decoding. The default value could change any time. If you care about this, then set it. (integer) 
-- MPG123_FEEDBUFFER Minimal size of one internal feeder buffer, again, the default value is subject to change. (integer) 


data Mpg123_parms = PVerbose | PFlags | PAddFlags | PForceRate | PDownSample | PRva | PDownSpeed | PUpSpeed | PStartFrame
  | PDecodeFrames | PIcyInterval | POutScale | PTimeout | PRemoveFlags | PResyncLimit | PIndexSize | PPreFrames | PFeedPool
  | PFeedBuffer
  deriving (Eq, Show, Enum)


-- * Error codes

{-
    MPG123_DONE 	 Message: Track ended. Stop decoding.
    MPG123_NEW_FORMAT 	 Message: Output format will be different on next call. Note that some libmpg123 versions between 1.4.3 and 1.8.0 insist on you calling mpg123_getformat() after getting this message code. Newer verisons behave like advertised: You have the chance to call mpg123_getformat(), but you can also just continue decoding and get your data.
    MPG123_NEED_MORE 	 Message: For feed reader: "Feed me more!" (call mpg123_feed() or mpg123_decode() with some new input data).
    MPG123_ERR 	Generic Error
    MPG123_OK 	Success
    MPG123_BAD_OUTFORMA  Unable to set up output format!
    MPG123_BAD_CHANNEL 	Invalid channel number specified.
    MPG123_BAD_RATE 	Invalid sample rate specified.
    MPG123_ERR_16TO8TABLE Unable to allocate memory for 16 to 8 converter table!
    MPG123_BAD_PARAM 	Bad parameter id!
    MPG123_BAD_BUFFER 	Bad buffer given – invalid pointer or too small size.
    MPG123_OUT_OF_MEM 	Out of memory – some malloc() failed.
    MPG123_NOT_INITIALIZED 	You didn't initialize the library!
    MPG123_BAD_DECODER 	Invalid decoder choice.
    MPG123_BAD_HANDLE 	Invalid mpg123 handle.
    MPG123_NO_BUFFERS 	Unable to initialize frame buffers (out of memory?).
    MPG123_BAD_RVA 	Invalid RVA mode.
    MPG123_NO_GAPLESS 	This build doesn't support gapless decoding.
    MPG123_NO_SPACE 	Not enough buffer space.
    MPG123_BAD_TYPES 	Incompatible numeric data types.
    MPG123_BAD_BAND 	Bad equalizer band.
    MPG123_ERR_NULL 	Null pointer given where valid storage address needed.
    MPG123_ERR_READER 	Error reading the stream.
    MPG123_NO_SEEK_FROM_END 	Cannot seek from end (end is not known).
    MPG123_BAD_WHENCE 	Invalid 'whence' for seek function.
    MPG123_NO_TIMEOUT 	Build does not support stream timeouts.
    MPG123_BAD_FILE 	File access error.
    MPG123_NO_SEEK 	Seek not supported by stream.
    MPG123_NO_READER 	No stream opened.
    MPG123_BAD_PARS 	Bad parameter handle.
    MPG123_BAD_INDEX_PAR 	Bad parameters to mpg123_index() and mpg123_set_index()
    MPG123_OUT_OF_SYNC 	Lost track in bytestream and did not try to resync.
    MPG123_RESYNC_FAIL 	Resync failed to find valid MPEG data.
    MPG123_NO_8BIT 	No 8bit encoding possible.
    MPG123_BAD_ALIGN 	Stack aligmnent error
    MPG123_NULL_BUFFER 	NULL input buffer with non-zero size...
    MPG123_NO_RELSEEK 	Relative seek not possible (screwed up file offset)
    MPG123_NULL_POINTER 	You gave a null pointer somewhere where you shouldn't have.
    MPG123_BAD_KEY 	Bad key value given.
    MPG123_NO_INDEX 	No frame index in this build.
    MPG123_INDEX_FAIL 	Something with frame index went wrong.
    MPG123_BAD_DECODER_SETUP 	Something prevents a proper decoder setup
    MPG123_MISSING_FEATURE 	This feature has not been built into libmpg123.
    MPG123_BAD_VALUE 	A bad value has been given, somewhere.
    MPG123_LSEEK_FAILED 	Low-level seek failed.
    MPG123_BAD_CUSTOM_IO 	Custom I/O not prepared.
    MPG123_LFS_OVERFLOW 	Offset value overflow during translation of large file API calls – your client program cannot handle that large file.
    MPG123_INT_OVERFLOW 	Some integer overflow. 
-}

data Mpg123_errors = EDone | ENewFormat | ENeedMore | EErr | EOk | EBadOutFormat
  | EBadChannel | EBadRate | EErr16To8Table | EBadParam | EBadBuffer | EOutOfMem
  | ENotInitialized | EBadDecoder | EBadHandle | ENoBuffers | EBadRVA | ENoGapless
  | ENoSpace | EBadTypes | EBadBand | EErrNull | EErrReader | ENoSeekFromEnd
  | EBadWhence | ENoTimeout | EBadFile | ENoSeek | ENoReader | EBadPars | EBadIndexPar
  | EOutOfSync | EResyncFail | ENo8Bit | EBadAlign | ENullBuffer | ENoRelSeek
  | ENullPointer | EBadKey | ENoIndex | EIndexFail | EBadDecoderSetup | EMissingFeature
  | EBadValue | ELSeekFailed | EBadCustomIO | ELFSOverflow | EIntOverflow
  deriving (Eq, Show, Enum, Generic)
instance Exception Mpg123_errors where





  
-- * Flags

{-
enum mpg123_param_flags
 {
  MPG123_FORCE_MONO = 0x7
  ,MPG123_MONO_LEFT = 0x1
  ,MPG123_MONO_RIGHT = 0x2
  ,MPG123_MONO_MIX = 0x4
  ,MPG123_FORCE_STEREO = 0x8
  ,MPG123_FORCE_8BIT = 0x10
  ,MPG123_QUIET = 0x20
  ,MPG123_GAPLESS = 0x40
  ,MPG123_NO_RESYNC = 0x80
  ,MPG123_SEEKBUFFER = 0x100
  ,MPG123_FUZZY = 0x200
  ,MPG123_FORCE_FLOAT = 0x400
  ,MPG123_PLAIN_ID3TEXT = 0x800
  ,MPG123_IGNORE_STREAMLENGTH = 0x1000
  ,MPG123_SKIP_ID3V2 = 0x2000
  ,MPG123_IGNORE_INFOFRAME = 0x4000
  ,MPG123_AUTO_RESAMPLE = 0x8000
  ,MPG123_PICTURE = 0x10000
  ,MPG123_NO_PEEK_END = 0x20000
  ,MPG123_FORCE_SEEKABLE = 0x40000
  ,MPG123_STORE_RAW_ID3 = 0x80000
  ,MPG123_FORCE_ENDIAN = 0x100000
  ,MPG123_BIG_ENDIAN = 0x200000
 };
-}
  
data ParamFlags = FForceMono | FMonoLeft | FMonoRight | FMonoMix | FForceStereo
  | FForce8Bit | FQuiet | FGapless | FNoResync | FSeekBuffer | FFuzzy | FForceFloat
  | FPlainId3Text | FIgnoreStreamLength | FSkipId3V2 | FIgnoreInfoFrame
  | FAutoResample | FPicture | FNoPeekEnd | FForceSeekable | FStoreRawId3
  | FForceEndian | FBigEndian
  deriving (Eq, Show, Enum) 
  


-- * MPG Frame information ( http://mpg123.de/api/structmpg123__frameinfo.shtml )

data MpgVersion = MpgV1 | MpgV2 | MpgV3 deriving (Eq, Show, Enum)
-- instance Storable MpgVersion where
--   alignment _ = 2 

data MpgMode = Stereo | JointStereo | DualChannel | Mono deriving (Eq, Show, Enum)

data MpgFlags = Crc | Copyright | Private | Original deriving (Eq, Show, Enum)

data MpgVBR = CBR | VBR | ABR deriving (Eq, Show, Enum)


data MpgFrameInfo = MpgFrameInfo {
    mpgVer :: MpgVersion
  , mpgLayer :: C.CInt
  , mpgRate :: C.CLong
  , mpgMode :: MpgMode
  , mpgModeExt :: C.CInt
  , mpgFrameSize :: C.CInt
  , mpgFlags :: MpgFlags
  , mpgEmph :: C.CInt
  , mpgBitRate :: C.CInt
  , mpgABRRate :: C.CInt
  , mpgVBR :: MpgVBR } deriving (Eq, Show)

-- instance Storable MpgFrameInfo where


-- * inline-c type mapping

data Mpg123_handle = Mpg123_handle


mpg123TypesTable :: M.Map C.TypeSpecifier TH.TypeQ
mpg123TypesTable = M.fromList [
  (C.TypeName "mpg123_handle", [t| Mpg123_handle |]),
  (C.TypeName "off_t", [t| COff |]),
  (C.TypeName "mpg123_frameinfo", [t| MpgFrameInfo |])
                              ]

