{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Codec.Mpg123.Internal.InlineC where

import Language.C.Inline.Context (ctxTypesTable, baseCtx, funCtx, vecCtx, bsCtx)
import qualified Language.C.Types as C
import qualified Language.C.Inline as C

import qualified Language.Haskell.TH as TH

import Data.Monoid

import qualified Data.Map as M


C.include "<mpg123.h>"

-- | API : https://www.mpg123.de/api/group__mpg123__init.shtml

mpg123Ctx :: C.Context
mpg123Ctx = baseCtx <> funCtx <> vecCtx <> ctx where
  ctx = mempty { ctxTypesTable = mpg123TypesTable}

data Mpg123_handle = Mpg123_handle


-- enum mpg123_parms
-- 166 	{
-- 167 	        MPG123_VERBOSE = 0,        /**< set verbosity value for enabling messages to stderr, >= 0 makes sense (integer) */
-- 168 	        MPG123_FLAGS,          /**< set all flags, p.ex val = MPG123_GAPLESS|MPG123_MONO_MIX (integer) */
-- 169 	        MPG123_ADD_FLAGS,      /**< add some flags (integer) */
-- 170 	        MPG123_FORCE_RATE,     /**< when value > 0, force output rate to that value (integer) */
-- 171 	        MPG123_DOWN_SAMPLE,    /**< 0=native rate, 1=half rate, 2=quarter rate (integer) */
-- 172 	        MPG123_RVA,            /**< one of the RVA choices above (integer) */
-- 173 	        MPG123_DOWNSPEED,      /**< play a frame N times (integer) */
-- 174 	        MPG123_UPSPEED,        /**< play every Nth frame (integer) */
-- 175 	        MPG123_START_FRAME,    /**< start with this frame (skip frames before that, integer) */ 
-- 176 	        MPG123_DECODE_FRAMES,  /**< decode only this number of frames (integer) */
-- 177 	        MPG123_ICY_INTERVAL,   /**< stream contains ICY metadata with this interval (integer) */
-- 178 	        MPG123_OUTSCALE,       /**< the scale for output samples (amplitude - integer or float according to mpg123 output format, normally integer) */
-- 179 	        MPG123_TIMEOUT,        /**< timeout for reading from a stream (not supported on win32, integer) */
-- 180 	        MPG123_REMOVE_FLAGS,   /**< remove some flags (inverse of MPG123_ADD_FLAGS, integer) */
-- 181 	        MPG123_RESYNC_LIMIT,   /**< Try resync on frame parsing for that many bytes or until end of stream (<0 ... integer). This can enlarge the limit for skipping junk on beginning, too (but not reduce it).  */
-- 182 	        MPG123_INDEX_SIZE      /**< Set the frame index size (if supported). Values <0 mean that the index is allowed to grow dynamically in these steps (in positive direction, of course) -- Use this when you really want a full index with every individual frame. */
-- 183 	        ,MPG123_PREFRAMES /**< Decode/ignore that many frames in advance for layer 3. This is needed to fill bit reservoir after seeking, for example (but also at least one frame in advance is needed to have all "normal" data for layer 3). Give a positive integer value, please.*/
-- 184 	        ,MPG123_FEEDPOOL  /**< For feeder mode, keep that many buffers in a pool to avoid frequent malloc/free. The pool is allocated on mpg123_open_feed(). If you change this parameter afterwards, you can trigger growth and shrinkage during decoding. The default value could change any time. If you care about this, then set it. (integer) */
-- 185 	        ,MPG123_FEEDBUFFER /**< Minimal size of one internal feeder buffer, again, the default value is subject to change. (integer) */
-- 186 	};

data Mpg123_parms = PVerbose | PFlags | PAddFlags | PForceRate | PDownSample | PRva | PDownSpeed | PUpSpeed | PStartFrame | PDecodeFrames | PIcyInterval | POutScale | PTimeout | PRemoveFlags | PResyncLimit | PIndexSize | PPreFrames | PFeedPool | PFeedBuffer deriving (Eq, Show, Enum)


mpg123TypesTable :: M.Map C.TypeSpecifier TH.TypeQ
mpg123TypesTable = M.fromList [
  (C.TypeName "mpg123_handle", [t| Mpg123_handle |])
                              ]



-- nagCtx :: Context
-- nagCtx = baseCtx <> funCtx <> vecCtx <> ctx
--   where
--     ctx = mempty
--       { ctxTypesTable = nagTypesTable
--       }





-- nagTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
-- nagTypesTable = Map.fromList
--   [ -- TODO this might not be a long, see nag_types.h
--     (C.TypeName "Integer", [t| Nag_Integer |])
--   , (C.TypeName "Complex", [t| Complex |])
--   , (C.TypeName "NagError", [t| NagError |])
--   , (C.TypeName "Nag_Boolean", [t| Nag_Boolean |])
--   , (C.TypeName "Nag_Comm", [t| Nag_Comm |])
--   , (C.TypeName "Nag_User", [t| Nag_User |])
--   , (C.TypeName "Nag_ErrorControl", [t| Nag_ErrorControl |])
-- ]
