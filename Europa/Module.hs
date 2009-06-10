-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL
--
-- A representation of module names and associated functions to map module
-- names to source files and vice-versa.
module Europa.Module
    ( -- * Data types
      Hierarchy(..), Module(..)
    -- * Exceptions
    , InvalidModuleName(..)
    -- * Functions
    , hierarchy, toList
    , pathFromModule, moduleFromPath
    , srcPathFromModule, objPathFromModule, ifacePathFromModule
    -- * Qualified names.
    , Qid(..), qid, (.$), provenance
    ) where

import System.FilePath
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Typeable
import Europa.EuM

data Hierarchy = !Hierarchy :. !B.ByteString | Root
                 deriving (Eq, Ord, Show)

newtype Module = Module Hierarchy
    deriving (Eq, Ord, Show)

newtype InvalidModuleName = InvalidModuleName String
    deriving (Eq, Ord, Typeable)

instance Show InvalidModuleName where
    show (InvalidModuleName name) = "invalid character in " ++ name

instance Exception InvalidModuleName

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
check cmpt@(x:xs) | isAlpha x, and (map isAlphaNum xs) = cmpt
                  | otherwise = throw $ InvalidModuleName cmpt

pathFromModule :: String -> Module -> FilePath
pathFromModule ext (Module mod) =
    addExtension (joinPath $ map B.unpack $ toList mod) ext

moduleFromPath :: FilePath -> Module
moduleFromPath =
    Module . hierarchy . map (B.pack . check) . splitDirectories . dropExtension

srcPathFromModule :: Module -> FilePath
srcPathFromModule = pathFromModule ".eu"

objPathFromModule :: Module -> FilePath
objPathFromModule = pathFromModule ".euo"

ifacePathFromModule :: Module -> FilePath
ifacePathFromModule = pathFromModule ".eui"

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
provenance :: Qid -> Maybe Module
provenance (Qid Root _ _) = Nothing
provenance (Qid qual _ _) = Just (Module qual)
