module Europa.Analysis.Rule where

import Europa.Core
import Europa.Module
import Europa.EuM
import qualified Europa.Rule as Rule
import Europa.Pretty ()
import Text.PrettyPrint.Leijen hiding (group)
import Data.List (group, sort)


newtype NonContiguousRules = NonContiguousRules Qid
    deriving (Eq, Ord, Typeable)

instance Show NonContiguousRules where
    show (NonContiguousRules id) =
        show (text "Rules for" <+> pretty id <+> text "should be given contiguously.")

instance Exception NonContiguousRules

checkOrdering :: [TyRule Qid a] -> EuM ()
checkOrdering rules = do
  mapM_ (\x -> when (length x > 1) (throw $ NonContiguousRules (head x))) $
        group $ sort $ map head $ group $ map Rule.headConstant rules

newtype BadPattern = BadPattern [Qid]
    deriving (Eq, Ord, Typeable)

instance Show BadPattern where
    show (BadPattern ids) =
        let ppvars = sep (punctuate comma (map pretty ids))
        in show (text "Pattern variables" <+> ppvars <+> text "cannot appear in constructor position.")

instance Exception BadPattern

checkHead :: TyRule Qid a -> EuM ()
checkHead (env :@ lhs :--> rhs) =
    let bad = [ x | App (Var x _) _ _ <- everyone lhs, x `isin` env ]
    in when (not (null bad)) $ throw (BadPattern bad)
