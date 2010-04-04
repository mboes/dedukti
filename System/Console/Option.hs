-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Parse command-line options (or flags).
--
-- Flag descriptions obey the following conventions:
--
-- * double hyphen flags don't have arguments.
--
-- * Everything after the first letter of a hyphen flag is part of the
-- argument.

module System.Console.Option where

import Text.PrettyPrint.Leijen
import Data.Either (partitionEithers)
import Data.List (partition, isPrefixOf)
import System.IO


data FlagArity f = Nullary String f
                 | Unary String (String -> f)

type Description f = [([FlagArity f], String)]

parseCmdline :: Description f -> [String] -> ([f], [String], [String])
parseCmdline desc args =
  let (hyphened, rest) = partition (\arg -> "-" `isPrefixOf` arg) args
      (errors, flags) = partitionEithers $ map toFlag hyphened
  in (flags, rest, errors)
    where flagmap = concatMap fst desc
          unpack ('-':'-':name) = (name, "")
          unpack ('-':name:arg) = ([name], arg)
          unpack x = error $ "Malformed argument: " ++ x
          lookupFlag name arg (Nullary n f : desc)
              | fst (unpack n) == name =
                  if null arg
                  then Right f
                  else Left $ "No argument expected for flag " ++ n
          lookupFlag name arg (Unary n f : desc)
              | fst (unpack n) == name = Right (f arg)
          lookupFlag name _ [] = Left $ "Flag not found: " ++ name
          lookupFlag name arg (_:desc) = lookupFlag name arg desc
          toFlag x | (name, arg) <- unpack x = lookupFlag name arg flagmap

printUsage :: Doc -> IO ()
printUsage = hPutStrLn stderr . show

printHelp :: Doc -> Description f -> IO ()
printHelp usage desc = do
  let flags = vsep (map pflag desc)
  putStrLn $ show $ usage <$> text "Options:" <$> indent 4 flags
    where pflag (flags, desc) = fillBreak 14 (hcat $ punctuate (text ", ") $
                                   map (text . name) flags) <+> text desc
          name (Nullary n _) = n
          name (Unary n _) = n
