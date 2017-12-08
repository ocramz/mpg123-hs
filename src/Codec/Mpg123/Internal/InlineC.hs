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
