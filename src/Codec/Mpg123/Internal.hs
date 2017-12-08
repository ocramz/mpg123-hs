{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Codec.Mpg123.Internal where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import System.Posix.Types

import           Foreign.Storable (Storable(..))
import qualified Language.Haskell.TH as TH

import qualified Language.C.Inline as C
import Language.C.Inline.Context (ctxTypesTable, baseCtx, funCtx, vecCtx, bsCtx)
import qualified Language.C.Types as C

import Data.Monoid

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
-- mpg123handle mh = [C.exp| void{ mpg123_handle($mh)}|]

-- MPG123_EXPORT int 	mpg123_param (mpg123_handle *mh, enum mpg123_parms type, long value, double fvalue)
-- MPG123_EXPORT int 	mpg123_getparam (mpg123_handle *mh, enum mpg123_parms type, long *value, double *fvalue)
-- MPG123_EXPORT int 	mpg123_feature (const enum mpg123_feature_set key)





-- nagCtx :: Context
-- nagCtx = baseCtx <> funCtx <> vecCtx <> ctx
--   where
--     ctx = mempty
--       { ctxTypesTable = nagTypesTable
--       }


mpg123Ctx = baseCtx <> funCtx <> vecCtx <> ctx where
  ctx = mempty { ctxTypesTable = mpg123TypesTable}

data Mpg123_handle = Mpg123_handle

mpg123TypesTable = M.fromList [
  (C.TypeName "mpg123_handle", [t| Mpg123_handle|])
                              ]


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
