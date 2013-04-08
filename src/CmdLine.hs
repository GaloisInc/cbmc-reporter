{-# LANGUAGE NamedFieldPuns #-}

module CmdLine where

import Data.List (intersperse)
import System.Console.GetOpt
import Text.PrettyPrint

--------------------------------------------------------------------------------
-- Command line interaction

class PShow a where
  pshow :: a -> String

class PRead a where
  pread :: String -> a

data Format = Markdown | ASCII
  deriving (Show, Read, Eq)

instance PShow Format where
  pshow s = case s of
              Markdown -> "markdown"
              ASCII    -> "ascii"

instance PRead Format where
  pread s = case s of
              "markdown" -> Markdown
              "ascii"    -> ASCII
              _          -> error $ "Cannot parse " ++ s

data Entries = Min | All
  deriving (Show, Read, Eq)

instance PShow Entries where
  pshow s = case s of
              Min -> "min-entries"
              All -> "all-entries"

instance PRead Entries where
  pread s = case s of
              "min-entries" -> Min
              "all-entries" -> All
              _          -> error $ "Cannot parse " ++ s

-- Sort order
data Sort = File | Func | Line | Result | Entry
  deriving (Eq, Read, Show)

instance PShow Sort where
  pshow s = case s of
              File   -> "file"
              Func   -> "function"
              Line   -> "line"
              Result -> "result"
              Entry  -> "entry"

instance PRead Sort where
  pread s = case s of
              "file"     -> File
              "function" -> Func
              "line"     -> Line
              "result"   -> Result
              "entry"    -> Entry
              _          -> error $
                              "No parse for " ++ s ++ " into a sort order."

data Opts = Opts
 { cbmc     :: FilePath       -- ^ CBMC executable.  If not, assume it's in the
                              -- path.
 , srcs     :: [String]       -- ^ C sources.
 , incls    :: [String]       -- ^ Includes.
 , function :: [String]       -- ^ functions to analyze.
 , cbmcOpts :: [String]       -- ^ Remaining options
 , format   :: Format         -- ^ Output format
 , timeout  :: Int            -- ^ Timeout bound in seconds
 , asserts  :: Bool           -- ^ Show the assertions in the generated report?
 , coverage :: Entries        -- ^ Minimal set coverage or run from all entries?
 , outfile  :: Maybe FilePath -- ^ A file to write the generated table
 , sort     :: Sort           -- ^ Column to sort the generated table.
 , help     :: Bool
 -- Internally used options
 , toExec   :: FilePath       -- ^ Timeout executable
  } deriving (Eq, Read, Show)

defaultOpts :: Opts
defaultOpts  = Opts
  { cbmc     = "cbmc"
  , srcs     = []
  , incls    = []
  , function = []
  , cbmcOpts = []
  , format   = ASCII
  , timeout  = 0 -- No timeout
  , asserts  = True
  , coverage = Min
  , outfile  = Nothing -- default: standard out
  , sort     = File
  , help     = False
  , toExec   = ""
  }

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option "c"  ["cbmc"]       cbmcOpt  cbmcHlp
  , Option "f"  ["function"]   funcOpt  funcHlp
  , Option "s"  ["src"]        srcOpt   srcHlp
  , Option "I"  ["incl"]       inclOpt  inclHlp
  , Option "m"  ["format"]     fmtOpt   fmtHlp
  , Option "t"  ["timeout"]    timeOpt  timeHlp
  , Option "n"  ["no-asserts"] astOpt   astHlp
  , Option "e"  ["entries"]    entOpt   entHlp
  , Option "o"  ["outfile"]    outOpt   outHlp
  , Option "r"  ["sort"]       sortOpt  sortHlp
  , Option "h"  ["help"]       helpOpt  helpHlp
  ]
  where
  cbmcOpt = ReqArg (\cbmc opts -> opts {cbmc}) "PATH"
  cbmcHlp = unwords
    [ "Optional path to the CBMC executable."
    , "If no path is provided, it is assumed"
    , "to exist in your $PATH." ]

  funcOpt = ReqArg (\f opts -> opts {function = f:function opts}) "SYMBOL"
  funcHlp = unwords
    [ "Entry functions for analysis."
    , "Multiple entry points may be given"
    , "as --function=f0 --function=f1 ..."
    , "If no functions are given, function"
    , "symbols are parsed from the C sources."
    , "Model-checking from each function is"
    , "run in parallel (modulo the number of"
    , "cores available."
    ]

  entOpt = ReqArg (\ent opts -> opts {coverage = pread ent})
             (pshow Min ++ "|" ++ pshow All)
  entHlp = unwords
    [ "Which entry functions to test with"
    , "for claims that are reachable from"
    , "more than one entry point."
    , "min-entries finds approximates a minimum"
    , "number of required entry points using a"
    , "depth-first greedy set-cover approximation."
    , "all-entries uses all functions as entries.  If a claim"
    , "fails from any entry point, False is reported"
    , "for the claim (and the entry function used)."
    , "Default: min-entries."
    ]

  srcOpt = ReqArg (\src opts -> opts {srcs = src:srcs opts}) "PATH"
  srcHlp = unwords
    [ "PATH to C source file."
    , "Multiple sources may be provided."
    ]

  inclOpt = ReqArg (\incl opts -> opts {incls = incl:incls opts}) "PATH"
  inclHlp = unwords
    [ "PATH to includes. "
    , "Multiple includes may be provided."
    , "PATH should NOT include the preceding -I."
    ]

  fmtOpt = ReqArg (\fmt opts -> opts {format = pread fmt})
             (pshow ASCII ++ "|" ++ pshow Markdown)
  fmtHlp = unwords
    [ "Output format of table."
    , "Default: ASCII format."]

  timeOpt = ReqArg (\t opts -> opts {timeout = read t}) "INT"
  timeHlp = unwords
    [ "Timeout bound for CBMC"
    , "(in seconds)." ]

  astOpt = NoArg (\opts -> opts {asserts = False})
  astHlp = unwords
    [ "Do not show the proposed assertion"
    , "expressions in the generated report."
    , "Default: False."
    ]

  outOpt = ReqArg (\fp opts -> opts {outfile = Just fp}) "PATH"
  outHlp = unwords
    [ "Path to write generated table."
    , "Default: stdout."
    ]

  sortOpt = ReqArg (\srt opts -> opts {sort = pread srt})
             ( concat
             $ intersperse "|"
             $ map pshow [File, Func, Line, Result, Entry]
             )
  sortHlp = unwords
    [ "Column to sort table (low-to-high)."
    , "For each claim: file name, function,"
    , "line number, entry point, or result"
    , "(true or false).  Default: file."
    ]

  helpOpt = NoArg (const defaultOpts {help = True})
  helpHlp = ""

parseArgs :: [String] -> Either [String] Opts
parseArgs args =
  case getOpt RequireOrder options args of
    (opts,toCbmc,[]) -> Right $ foldl (flip id) (startOpts toCbmc) opts
    (_,_,errs)       -> Left errs
  where
  startOpts cbmcOpts = defaultOpts {cbmcOpts}

header :: String
header = unlines
  [ ""
  , "-------------------------------------------------------------"
  , "| cbmc-reporter is a driver for the CBMC model-checker      |"
  , "| <http://www.cprover.org/cbmc/> for use with library code. |"
  , "|                                                           |"
  , "| Licence   : BSD3                                          |"
  , "| Maintainer: Lee Pike (leepike@galois.com)                 |"
  , "|                                                           |"
  , "| Usage     : cbmc-report [OPTION...] -- [CBMC OPTIONS ...] |"
  , "| (All CBMC options must come after cbmc-report options).   |"
  , "-------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------

-- A better usageInfo formatter.
myUsageInfo :: String
myUsageInfo = renderStyle style' $
     text header
  $$ empty
  $$ text "USAGE:"
  $$ vcat (map docOpt options)
  $$ text "EXAMPLES:"
  $+$ space $$ examples
  $+$ space

  where
  style' = style {lineLength = 70}
  docOpt (Option shrt [long] req hlp) =
       nest 2 $ text "-" <> text shrt <> args req space <> comma
              <+> text "--" <> text long <> args req equals
    $$ nest 4 (takeup 0 empty (words hlp)) $+$ space
  docOpt _ = empty

  ll = lineLength style'

  -- Take words up to lineLength words, keeping the remainder.
  takeup :: Int -> Doc -> [String] -> Doc
  takeup _ acc []         = acc
  takeup n acc (wrd:wrds) =
    let len = n + length wrd + 1 in
    -- Can't fit anymore words on this line.
    if len >= ll then acc $$ takeup 0 empty (wrd:wrds)
      else takeup len (acc <+> text wrd) wrds

  args req sp = case req of
                  (ReqArg _ arg) -> sp <> text arg
                  (NoArg _)      -> empty
                  -- OptArg ... if we add that
                  _              -> empty

  examples =
       nest 2 (text "> cbmc-reporter -f foo -f bar -s s0.c -s s1.c -I hdr.h -- --win64")
    $$ nest 4 (takeup 0 empty $ words explain)
    where
    explain = unwords
      [ "Using foo and bar as entry points, analyze s0 and s1 with"
      , "header hdr.h.  Pass an an argument to CBMC --win64."
      ]
