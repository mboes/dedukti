-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- Pretty-printing of various core data types. This is not meant as a
-- replacement of the |Show| class but rather an alternative. You
-- should derive Show for all data types, then declare them instances
-- of |Pretty| where appropriate.

module Dedukti.Pretty (Pretty(pretty)) where

import Dedukti.Core
import Dedukti.Module
import Text.PrettyPrint.Leijen
import qualified Data.ByteString.Lazy.Char8 as B


textB = text . B.unpack

instance Pretty id => Pretty (Binding id a) where
    pretty (L x) = pretty x
    pretty (x ::: ty) = pretty x <+> char ':' <+> pretty ty
    pretty (x := t) = pretty x <+> text ":=" <+> pretty t

    prettyList = vcat . map (\x -> pretty x <> dot)

instance Pretty id => Pretty (Expr id a) where
    pretty (B (L x) t _) = pretty x <+> text "=>" <+> pretty t
    pretty (B (x ::: dom@(B _ _ _)) ran _) =
      pretty x <+> char ':' <+> parens (pretty dom) <+> text "->" <+> pretty ran
    pretty (B (x ::: dom) ran _) =
      pretty x <+> char ':' <+> pretty dom <+> text "->" <+> pretty ran
    pretty (A t1 t2 _) =
        let f = if isApplicative t1 then id else parens
            g = if isAtomic t2 then id else parens
        in f (pretty t1) <+> g (pretty t2)
    pretty (V x _) = pretty x
    pretty Type = text "Type"
    pretty Kind = text "Kind"

instance Pretty id => Pretty (Rule id a) where
    pretty (lhs :--> rhs) = pretty lhs <+> text "-->" <+> pretty rhs

instance (Eq a, Ord id, Pretty id) => Pretty (TyRule id a) where
    pretty (env :@ rule)
        | env == emptyEnv = text "[]" <+> pretty rule
        | otherwise =
            encloseSep (text "[ ") (text " ]") (text ", ")
                           (map pretty (env_bindings env))
            <+> pretty rule

    prettyList = vcat . map (\x -> pretty x <> dot)

instance (Eq a, Ord id, Pretty id) => Pretty (RuleSet id a) where
    pretty RS{..} = vcat (pretty (rs_name ::: rs_type) : map pretty rs_rules)
    prettyList = vcat . punctuate line . map pretty

instance Pretty Qid where
    pretty qid = joinQ (qid_qualifier qid) <>
                 textB (fromAtom (qid_stem qid)) <>
                 joinS (qid_suffix qid)
        where joinQ Root = empty
              joinQ (h :. x) = joinQ h <> textB (fromAtom x) <> dot
              joinS Root = empty
              joinS (h :. x) = joinS h <> char '_' <> textB (fromAtom x)
