{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO:
-- XXX on error in thread, kill everything.
-- XXX prove each assertion only once?

-- Dependence on timeout.  Not clear how to remove it, since Haskell doesn't
-- have a good mechanism to guarantee killing an external process.


-- | Parses the (standard) output CBMC and prints to standard out for each
-- function: It's location, did it succeed or fail.  Filepaths must be relative
-- to the curent directory.

module Main

where

import ParseXML
import CmdLine
import ParseSrcs

import Prelude hiding (init)
import System.Exit
import System.Process
import System.Environment
import System.Directory
import Data.List hiding (sort, init)
import Data.Maybe
import Control.Monad
import Data.Monoid hiding (All)

import Control.Concurrent

import qualified Spreadsheet as S
import qualified Spreadsheet.Renderer as S
import qualified Spreadsheet.Sorting as S

import qualified MonadLib as M

--import Text.Show.Pretty
--import Debug.Trace

--------------------------------------------------------------------------------
-- Environment/execution

-- How we want to use CBMC.
data Output = ReachableClaims -- Shows just reachable claims from the entry point.
            | ShowClaims      -- Shows every claim in the all the sources.
            | Run             -- Run the model-checker.
  deriving (Eq, Show, Read)

instance PShow Output where
  pshow ReachableClaims = "--cover-assertions"
  pshow ShowClaims      = "--show-claims"
  pshow Run             = "analyzing claims"

-- Error messages and entry functions causing errors.
newtype Error = Error { errMsgs  :: [String] }
  deriving (Show, Read, Eq)

instance Monoid Error where
  mempty                        = Error []
  -- Errors probably shouln't get too big, so concatentation is efficient
  -- enough.
  (Error a) `mappend` (Error b) = Error (a ++ b)

--------------------------------------------------------------------------------
-- CBMC Monad and functions.

newtype CBMC a = CBMC { unCBMC :: M.WriterT Error IO a }
  deriving Monad

insertErr :: String -> CBMC ()
insertErr err = CBMC (M.put $ Error [err])

maybeInsertErr :: Maybe Error -> CBMC ()
maybeInsertErr = maybe (return ()) (CBMC . M.put)

liftIO :: IO a -> CBMC a
liftIO io = CBMC $ M.inBase io

putStrLnM :: String -> CBMC ()
putStrLnM = liftIO . putStrLn

--------------------------------------------------------------------------------
-- Misc type synonyms

-- Error, the function, and the output.
type ChanData = (Maybe Error, Func, String)

type FuncsCoverage = (Func, [ClaimName])

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left errs  -> ioError (userError $ unlines $ errs ++ [myUsageInfo])
    Right opts
      | help opts -> putStrLn myUsageInfo
      | otherwise -> do

      sources <- mapM makeRelativeToCurrentDirectory (srcs opts)
      let headers = map ("-I"++) (incls opts)
      functions <- case function opts of
                     [] -> extractSrcs sources headers
                     fs -> return fs
      let opts' = opts { cbmcOpts = cbmcOpts opts ++ headers ++ sources
                       , function = functions
                       }

      (claims, Error errs) <- M.runWriterT (unCBMC $ run opts')

      putStrLn " Generating spreadsheet ..."
      let spreadsheet | format opts' == ASCII
                      = S.renderSpreadsheet (spreadSheetize opts' claims)
                      | otherwise -- format opts' == Markdown
                      = S.renderSmd (spreadSheetize opts' claims)

      maybe ( putStrLn $ allOut spreadsheet claims errs )
            ( flip writeFile $ allOut spreadsheet claims errs )
            ( outfile opts' )
  where

  allOut ss clms errs  = unlines
                  $ ["", ss]
                 ++ ["", totals (cnt clms), ""]
                 ++ if null errs then [] else [outErrs errs]

  cnt = partition id . map claimVal
  totals (true,false) =
    "Analyzed " ++ show (length true) ++ " true " ++ claimStr true
        ++ " and " ++ show (length false) ++ " false "
        ++ claimStr false ++ "."
    where claimStr xs = if length xs == 1 then "claim" else "claims"

  outErrs        :: [String] -> String
  outErrs errs   = unlines $ "************"
                           : "* Warnings *"
                           : map ("\n  * "++) errs

-- Main driver function.
run :: Opts -> CBMC [Claim]
run opts@Opts {cbmc, function} = do
  cbmc' <- liftIO $ do f  <- doesFileExist cbmc
                       ex <- getExec "cbmc"
                       return $ if f then cbmc else ex
  timeout <- liftIO $ getExec "timeout"
  let opts' = opts {cbmc = cbmc', toExec = timeout}

  -- Find the functions that get all reachable claims.  XXX This algorithm just
  -- iterates over functions and does not not compute a maximum vertex cycle
  -- cover.
  putStrLnM " Getting all reachable claims ..."

  -- Minimal number of functions to cover all claims.
  entriesAndClaims <-
    spawnCBMC opts' ReachableClaims allReaches function []

  let minEntriesAndClaims =
        case coverage opts' of
          All  -> entriesAndClaims -- We prove starting from each function here.
          Min  -> minCoverageSet entriesAndClaims

  -- The only reason we need to run ShowClaims for analysis is because in the
  -- --xml-ui output of CBMC, for --cover-assertions, the line/file/function
  -- info isn't given, even though it's given in the non --xml-ui output.  We
  -- don't have to run this from a particular entry point, but CBMC requires
  -- *some* existing entry point (note the entry point doesn't even have to be
  -- analyzable (i.e., while(1) loop is fine).  *However*, we do use the results
  -- of this to provide the user with warnings about missed claims in the source
  -- files.
  allClaims       <-
    spawnCBMC opts' ShowClaims collectClaims (take 1 function) []
  reachableClaims <- keepReachableClaims allClaims minEntriesAndClaims

  -- Should contain no duplicates by default.
  let funcsMin = map fst minEntriesAndClaims
  putStrLnM " Analyzing claims ..."
  results <- spawnCBMC opts' Run collectResults funcsMin reachableClaims
  return $ nubResults results

  where

  -- Remove duplicated claims (from different starting functions), prioritizing
  -- false claims over true.
  nubResults reses = go [] reses
    where
    go clms [] = clms
    go clms rst@( Claim{ claimFile = cf0, claimFunc = cc0
                       , claimLine = cl0 }
                  : _
                )
      = go (getMaybeFalse : clms) nos
      where
      -- yeses can't be empty, by construction.
      (yes,nos) = partition (\Claim{ claimFile = cf1, claimFunc = cc1
                                   , claimLine = cl1 } ->
                             cf0 == cf1 && cc0 == cc1 && cl0 == cl1
                            ) rst
      getMaybeFalse :: Claim
      getMaybeFalse = fromMaybe (head yes) (find (not . claimVal) yes)

--------------------------------------------------------------------------------

spawnCBMC :: Opts
          -> Output
          -> (Chan ChanData -> [a] -> Int -> CBMC [a])
          -> [Func]
          -> [a]
          -> CBMC [a]
spawnCBMC opts outFormat folder funcs init = do
  chan <- liftIO newChan
  forM_ funcs (spawnThread chan)
  foldM (folder chan) init [1..length funcs]
  where
  spawnThread :: Chan ChanData -> Func -> CBMC ThreadId
  spawnThread chan func = do
    putStrLnM $ "  Spawning thread: "
              ++ pshow outFormat ++ " from function "
              ++ func ++ " ... "
    liftIO $ forkIO $ runCBMC chan outFormat opts func

--------------------------------------------------------------------------------

-- Initalized with all possible claims.
collectResults :: Chan ChanData
               -> [Claim]
               -> Int
               -> CBMC [Claim]
collectResults chan claims _ = do
  (merr, func, output) <- liftIO (readChan chan)
  maybeInsertErr merr
  let parsedRes        = parseResXML output
  reses               <- errNoParse [(func, parsedRes)]
  return               $ map (markFailure func reses) claims

markFailure :: Func -> [Result Failure] -> Claim -> Claim
markFailure entry reses c@Claim{claimFile, claimFunc, claimLine, claimEntry} =
  maybe c (const c {claimVal = False}) (find go reses)
  where
  go :: Result Failure -> Bool
  go res =
    case res of
      Succeeded -> False
      Failed Failure {failFile , failFunc , failLine} ->
              claimEntry == entry
           && claimFile  == failFile
           && claimFunc  == failFunc
           && claimLine  == failLine

--------------------------------------------------------------------------------

-- | Get location info for all reachable claims.
collectClaims :: Chan ChanData -> [Claim] -> Int -> CBMC [Claim]
collectClaims chan claims _ = do
  (merr, func, output)  <- liftIO (readChan chan)
  maybeInsertErr merr
  let parsedClaims      = parseClaimsXML output
  let claimsWithFunc    = zip (repeat func) parsedClaims
  assert                <- errNoParse claimsWithFunc
  return                $ assert ++ claims

--------------------------------------------------------------------------------

allReaches :: Chan ChanData
           -> [FuncsCoverage]
           -> Int
           -> CBMC [FuncsCoverage]
allReaches chan claims _ = do
  (merr, func, output) <- liftIO (readChan chan)
  asserts <- errNoParse $ zip (repeat func) (parseReachableXML output)
  -- Don't insert the function if we got an error processing it.
  return $ if isJust merr then claims else (func, asserts) : claims

--------------------------------------------------------------------------------
-- Running CBMC.

-- Entry point to call CBMC.
runCBMC :: Chan ChanData -> Output -> Opts -> Func -> IO ()
runCBMC chan outputType opts func = do
  (merr, output) <- getCBMCOutPut opts func outputType
  writeChan chan (merr, func, output)

-- Actually call out to CBMC.
getCBMCOutPut :: Opts -> Func -> Output -> IO (Maybe Error, String)
getCBMCOutPut opts@Opts {cbmc} func outputType = do
  -- No stderr output is given with XML mode.
  (exitCode,stdout,_) <- runIt allArgs
  processOut exitCode stdout
  where
  args = "--function":func:out
  allArgs = "--xml-ui" : args ++ cbmcOpts opts
  errArgs = args ++ cbmcOpts opts
  -- Timeout processes as necessary using timeout (GNU tool).
  duration = show (timeout opts) ++ "s"
  timeoutArgs args' = ("--kill-after=" ++ duration) : duration : cbmc : args'
  -- Actually call CBMC.
  runIt args' = readProcessWithExitCode (toExec opts) (timeoutArgs args') ""
  processOut exitCode stdout
    -- Timeout killed the process.
    |    exitCode == ExitFailure 124 -- 124 is the timeout exit code.
      || exitCode == ExitFailure 9   -- Hard kill.
    = return ( Just $ Error ["timeout: cbmc with args: " ++ unwords args]
             , stdout
             )
    -- Were there processing errors?
    | parseErrMsgsXML stdout
    -- Run the command again to get the error message, and throw a fatal error.
    = do putStrLn   "*** Fatal CBMC error encountered!"
         putStrLn   "*** (Rebuilding error ...)\n"
         (_,_,errout) <- runIt errArgs
         putStrLn $ "    Error: " ++ errout
         error "\n*** Ending run."
    | otherwise
    = return (Nothing, stdout)

  out = case outputType of
           ShowClaims      -> [pshow ShowClaims]
           ReachableClaims -> [pshow ReachableClaims]
           Run             -> []

--------------------------------------------------------------------------------
-- Helpers

errNoParse :: [(Func, Maybe a)]-> CBMC [a]
errNoParse = foldM go []
  where
  go acc (func,result)
    | Just res <- result = return (res:acc)
    | otherwise          = do insertErr $ "Can't parse output from func: "
                                       ++ func
                              return acc

getExec :: FilePath -> IO FilePath
getExec exec = do
  mfp <- findExecutable exec
  return $ fromMaybe (error $ "executable \""
                            ++ exec ++ "\" does not exist!")
                     mfp

--------------------------------------------------------------------------------
-- Analyzing claims

-- | Greedy algorithm implementing the minimum coverage set
-- <http://en.wikipedia.org/wiki/Set_cover_problem>.  Returns the minimal number
-- of entry-point functions covering all reachable claims.
minCoverageSet :: [FuncsCoverage] -> [FuncsCoverage]
minCoverageSet = minCoverageSet' []
  where
  minCoverageSet' :: [FuncsCoverage] -> [FuncsCoverage] -> [FuncsCoverage]
  minCoverageSet' minSet ls =
    case ls of
      [] -> minSet
      _  -> let minClms = nub $ concatMap snd minSet in
            let m = findMaxUncovered minClms ls in
            let go fc = minCoverageSet' (fc:minSet) (delete fc ls) in
            -- if m is Nothing, we've covered the set.
            maybe minSet go m

  -- return the max uncovered.
  findMaxUncovered :: [ClaimName] -> [FuncsCoverage] -> Maybe FuncsCoverage
  findMaxUncovered minSet candidates =
   case zipped of
     []    -> Nothing
     _     -> let (diff, cand) = maximumBy go zipped in
              if diff <= 0 then Nothing else Just cand
    where
    go a b = compare (fst a) (fst b)
    diffs  = map (diffCoveredClaim . snd) candidates
    zipped = zip diffs candidates

    -- How many elements in ls that aren't in minSet.
    diffCoveredClaim :: [ClaimName] -> Int
    diffCoveredClaim candidate = length $ candidate \\ minSet

eqClaim :: Claim -> Claim -> Bool
eqClaim c0 c1 = claimName c0 == claimName c1

eqClaimFailure :: Failure -> Claim -> Bool
eqClaimFailure
  Failure { failFile,  failFunc,  failLine  }
  Claim   { claimFile, claimFunc, claimLine }
  = claimFile == failFile && claimFunc == failFunc && claimLine == failLine

-- Get the meta data for reachable claims from the all-claims pass as well as
-- insert errors if there is an unreachable claim.  Also, put the entry function
-- inside the claim data.
keepReachableClaims :: [Claim] -> [FuncsCoverage] -> CBMC [Claim]
keepReachableClaims allClaims reachables = foldM go [] allClaims
  where
  reachableWEntries :: [(Func, ClaimName)]
  reachableWEntries = concatMap (\(f, cns) -> zip (repeat f) cns) reachables

  go :: [Claim] -> Claim -> CBMC [Claim]
  go reachableClaims allClaim =
    case withEntry of
      [] -> withErr
      _  -> insertClms withEntry
    where
    reses :: [(Func, ClaimName)]
    reses = filter (\(_,cn) -> claimName allClaim == cn)
                       reachableWEntries
    -- Make a new claim for each possibly entry.
    withEntry :: [Claim]
    withEntry = map (\(f,_) -> allClaim {claimEntry = f}) reses

    insertClms we = return $ we ++ reachableClaims
    withErr       = do insertErr $ "Did not cover claim "
                                ++ " in file "     ++ claimFile allClaim
                                ++ " in function " ++ claimFunc allClaim
                                ++ " at line "     ++ show (claimLine allClaim)
                       return reachableClaims

--------------------------------------------------------------------------------
-- | Prettyprint report
spreadSheetize :: Opts -> [Claim] -> S.Spreadsheet
spreadSheetize Opts {sort, asserts} claims
  = S.sortSpreadsheet $ S.Spreadsheet columns (transpose cellValues)
  where
  so = Just S.Ascending
  fileCol = S.Column "File"     S.StringT            so
  funcCol = S.Column "Function" S.StringT            so
  lineCol = S.Column "Line"     (S.NumberT Nothing)  so
  resCol  = S.Column "Result"   S.StringT            so
  entCol  = S.Column "Entry"    S.StringT            so
  astCol  = S.Column "Expr"     S.StringT            so

  initCol = case sort of
              File    -> (fileCol, fileVals)
              Func    -> (funcCol, funcVals)
              Line    -> (lineCol, lineVals)
              Result  -> (resCol , resVals)
              Entry   -> (entCol , entVals)

  cols = [fileCol, funcCol, lineCol, resCol, entCol]
  vals = [fileVals, funcVals, lineVals, resVals, entVals]

  (columns, cellValues) =
    unzip $ nub (initCol : zip cols vals)
         ++ if asserts then [(astCol, astVals)] else []

  fileVals = map (S.StringV . claimFile) claims
  funcVals = map (S.StringV . claimFunc) claims
  lineVals = map (S.NumberV . fromIntegral . claimLine) claims
  resVals  = map (S.StringV . show . claimVal) claims
  entVals  = map (S.StringV . claimEntry) claims
  astVals  = map (S.StringV . claimExpr) claims

--------------------------------------------------------------------------------
