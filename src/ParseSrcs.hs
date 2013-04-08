-- | Get function symbols from a list of C sources.  Calls the preprocessor as
-- needed.

module ParseSrcs
  ( extractSrcs
  ) where

import Language.C
import Language.C.System.GCC
import Language.C.Data.Ident

--------------------------------------------------------------------------------

-- Function specifier
type Func = String

--------------------------------------------------------------------------------

extractSrcs :: [FilePath] -> [FilePath] -> IO [Func]
extractSrcs srcs hdrs = do
  syms <- mapM (extractFunctions hdrs) srcs
  return $ concat syms

extractFunctions :: [FilePath] -> FilePath -> IO [Func]
extractFunctions hdrs src = do
  tr <- parseCFile (newGCC "gcc") Nothing hdrs src
  case tr of
    Left parseErr   -> error $ "Fatal parse error in extracting functions!\n\n"
                            ++ show parseErr
    Right transunit -> return $ findFuncs transunit

--------------------------------------------------------------------------------

findFuncs :: CTranslUnit -> [Func]
findFuncs (CTranslUnit decls _) = concatMap extractFunc decls
  where
  extractFunc (CDeclExt _   ) = []
  extractFunc (CAsmExt  _ _ ) = []
  extractFunc (CFDefExt func) = getDecl func

  getDecl (CFunDef _ decl _ _ _) = getSym decl

  getSym (CDeclr (Just ident) _ _ _ _) =
    case ident of
      Ident sym _ _ -> [sym]
  getSym _                             = []

--------------------------------------------------------------------------------


