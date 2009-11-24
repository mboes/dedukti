-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- A representation of module names and associated functions to map module
-- names to source files and vice-versa. Qualified names, as required in the
-- presence of modules, are also defined here.

module Dedukti.Module
    ( -- * Data types
      Hierarchy(..), MName
    -- * Exceptions
    , InvalidModuleName(..)
    -- * Functions
    , hierarchy, toList
    , pathFromModule, moduleFromPath
    , srcPathFromModule, objPathFromModule, ifacePathFromModule
    -- * Qualified names.
    , Qid(..), qid, (.$), provenance, unqualify
    ) where

import Dedukti.DkM
import System.FilePath
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.ByteString.Lazy.Char8 as B
import Text.PrettyPrint.Leijen


data Hierarchy = !Hierarchy :. !B.ByteString | Root
                 deriving (Eq, Ord, Show)

type MName = Hierarchy

newtype InvalidModuleName = InvalidModuleName String
    deriving (Eq, Ord, Typeable)

instance Show InvalidModuleName where
    show (InvalidModuleName name) = "invalid character in " ++ name

instance Exception InvalidModuleName

instance Pretty MName where
    pretty (Root :. x) = text (B.unpack x)
    pretty (xs :. x) = pretty xs <> char '.' <> text (B.unpack x)

hierarchy :: [B.ByteString] -> Hierarchy
hierarchy =  f . reverse where
    f [] = Root
    f (x:xs) = f xs :. x

toList :: Hierarchy -> [B.ByteString]
toList = reverse . f where
    f Root = []
    f (xs :. x) = x : f xs

-- | Raise an exception if module name component is a valid identifier.
check :: String -> String
check cmpt@(x:xs) | isAlpha x, and (map (\x -> isAlphaNum x || elem x "-+~") xs) = cmpt
                  | otherwise = throw $ InvalidModuleName cmpt

pathFromModule :: String -> MName -> FilePath
pathFromModule ext mod =
    addExtension (joinPath $ map B.unpack $ toList mod) ext

moduleFromPath :: FilePath -> MName
moduleFromPath =
    hierarchy . map (B.pack . check) . splitDirectories . dropExtension

srcPathFromModule :: MName -> FilePath
srcPathFromModule = pathFromModule ".dk"

objPathFromModule :: MName -> FilePath
objPathFromModule = pathFromModule ".dko"

ifacePathFromModule :: MName -> FilePath
ifacePathFromModule = pathFromModule ".dki"

-- | The datatype of qualified names.
data Qid = Qid { qid_qualifier :: !Hierarchy
               , qid_stem      :: !B.ByteString
               , qid_suffix    :: !Hierarchy }
           deriving (Eq, Ord, Show)

-- | Shorthand qid introduction.
qid :: B.ByteString -> Qid
qid x = Qid Root x Root

-- | Append suffix.
(.$) :: Qid -> B.ByteString -> Qid
(Qid qual x sufs) .$ suf = Qid qual x (sufs :. suf)

-- | Get the module where the qid is defined, based on its qualifier.
provenance :: Qid -> Maybe MName
provenance (Qid Root _ _) = Nothing
provenance (Qid qual _ _) = Just qual

-- | Remove any qualifier.
unqualify :: Qid -> Qid
unqualify qid = qid{qid_qualifier = Root}
