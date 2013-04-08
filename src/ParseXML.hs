{-# LANGUAGE NamedFieldPuns #-}

module ParseXML
  ( ClaimName
  , Func
  , Claim(..)
  , Result(..)
--  , Coverage(..)
  , Failure(..)
  , parseErrMsgsXML
  , parseReachableXML
  , parseClaimsXML
  , parseResXML
  ) where

import Data.Maybe
import Data.List

import qualified Text.XML.Light.Input as X
import qualified Text.XML.Light.Proc  as X
import qualified Text.XML.Light.Types as X

--------------------------------------------------------------------------------

type Func = String

type ClaimName = String

-- | A claim.
data Claim = Claim
  { claimName  :: ClaimName
  , claimFile  :: String
  , claimFunc  :: String
  , claimLine  :: Integer
  , claimExpr  :: String
  , claimVal   :: Bool
  , claimEntry :: Func -- Entry function name
  } deriving (Show, Read, Eq)

-- | The result of executing an assertion.
data Result a = Succeeded | Failed a
  deriving (Show, Read, Eq)

data Coverage a = Covered a | Uncovered
  deriving (Show, Read, Eq)

-- | Failed proof.
data Failure = Failure
  { failFile :: String
  , failFunc :: String
  , failLine :: Integer
  } deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- Parsing XML

-- | True if there are any error messages.
parseErrMsgsXML :: String -> Bool
parseErrMsgsXML out = not $ null $ catMaybes $ map findErrMsg msgs
  where
  msgs = concatMap (X.filterChildrenName $ matchQName "message")
                   (allElems out)

  findErrMsg msg = do
    mattrVal <- X.findAttrBy (("type" ==) . X.qName) msg
    if mattrVal == "ERROR" then return ()
      else Nothing

parseReachableXML :: String -> [Maybe ClaimName]
parseReachableXML out = foldl' covered [] resElems
  where
  resElems = concatMap (X.filterChildrenName $ matchQName "result")
                       (allElems out)
  covered :: [Maybe ClaimName] -> X.Element -> [Maybe ClaimName]
  covered clms el = maybe (Nothing:clms) (go clms) (extractClaim el)

  extractClaim :: X.Element -> Maybe (String, String)
  extractClaim el = do
    claimName <- findAttr "claim" el
    status    <- findAttr "status" el
    return (claimName, status)

  go :: [Maybe ClaimName] -> (String, String) -> [Maybe ClaimName]
  go clms (claimName, status) | status == "COVERED"     = Just claimName : clms
                              | status == "NOT_COVERED" = clms
                              | otherwise               = Nothing : clms

parseClaimsXML :: String -> [Maybe Claim]
parseClaimsXML out = map getClaimInfo claimElems
  where
  cproverElems = maybeToList $ getCproverElems out
  claimElems   =
    concatMap (X.filterChildrenName $ matchQName "claim") cproverElems

allElems :: String -> [X.Element]
allElems = X.onlyElems . X.parseXML

getLocInfo :: X.Element -> Maybe (String, String, Integer)
getLocInfo el = do
  locElem <- findChildElem "location" el
  fl      <- findAttr "file" locElem
  fun     <- findAttr "function" locElem
  ln      <- findAttr "line" locElem
  return (fl, fun, read ln)

getClaimInfo :: X.Element -> Maybe Claim
getClaimInfo el = do
  (claimFile, claimFunc, claimLine) <- getLocInfo el
  claimName <- findAttr "name" el
  expElem   <- findChildElem "expression" el
  expr      <- listToMaybe $ X.onlyText $ X.elContent expElem
  let claimExpr = X.cdData expr
  -- Just assume it's true to begin with.
  return Claim { claimName
               , claimFile
               , claimFunc
               , claimLine
               , claimExpr
               , claimVal = True
               , claimEntry = "" -- We'll add the entry function when we analyze
                                 -- it
               }

parseResXML :: String -> Maybe (Result Failure)
parseResXML out = do
  cproverElem <- getCproverElems out
  status      <- findChildElem "cprover-status" cproverElem
  getStatus cproverElem status

  where
  getStatus :: X.Element -> X.Element -> Maybe (Result Failure)
  getStatus cproverElem status =
    case X.strContent status of
      "FAILURE" -> do failureElem <- getFailureElem cproverElem
                      (failFile, failFunc, failLine) <- getLocInfo failureElem
                      return $ Failed $ Failure { failFile, failFunc, failLine }
      "SUCCESS" -> return Succeeded
      _         -> Nothing

getCproverElems :: String -> Maybe X.Element
getCproverElems = getElemList "cprover" . allElems

findAttr :: String -> X.Element -> Maybe String
findAttr attrNm = X.findAttrBy (matchQName attrNm)

getFailureElem :: X.Element -> Maybe X.Element
getFailureElem elm =
       findChildElem "goto_trace" elm
   >>= findChildElem "failure"

findChildElem :: String -> X.Element -> Maybe X.Element
findChildElem nm = X.filterChildName (matchQName nm)

-- Get a matching element from a list of elements
getElemList :: String -> [X.Element] -> Maybe X.Element
getElemList nm = find (matchQName nm . X.elName)

matchQName :: String -> X.QName -> Bool
matchQName nm qn = nm == X.qName qn

--------------------------------------------------------------------------------
