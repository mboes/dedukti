-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Various checks on rules.

module Dedukti.Analysis.Rule where

import Dedukti.Core
import Dedukti.Module
import Dedukti.DkM
import qualified Dedukti.Rule as Rule
import Dedukti.Pretty ()
import Text.PrettyPrint.Leijen hiding (group)
import Data.List (group, sort)


newtype NonContiguousRules = NonContiguousRules Qid
    deriving (Eq, Ord, Typeable)

instance Show NonContiguousRules where
    show (NonContiguousRules id) =
        show (text "Rules for" <+> pretty id <+> text "should be given contiguously.")

instance Exception NonContiguousRules

-- | All rules for a same constant must be given contiguously, without
-- intervening rules for other constants.
checkOrdering :: [TyRule Qid a] -> DkM ()
checkOrdering rules = do
  say Verbose $ text "Checking rule contiguity ..."
  mapM_ (\x -> when (length x > 1) (throw $ NonContiguousRules (head x))) $
        group $ sort $ map head $ group $ map Rule.headConstant rules

newtype BadPattern = BadPattern [Qid]
    deriving (Eq, Ord, Typeable)

instance Show BadPattern where
    show (BadPattern ids) =
        let ppvars = sep (punctuate comma (map pretty ids))
        in show (text "Pattern variables" <+> ppvars <+> text "cannot appear in constructor position.")

instance Exception BadPattern

checkHead :: TyRule Qid a -> DkM ()
checkHead (env :@ lhs :--> rhs) =
    let bad = [ x | App (Var x _) _ _ <- everyone lhs, x `isin` env ]
    in when (not (null bad)) $ throw (BadPattern bad)
