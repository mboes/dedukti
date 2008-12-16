module Europa.Pretty (pretty) where

import Europa.Core
import Text.PrettyPrint.Leijen


instance Pretty id => Pretty (Expr id a) where
    pretty (Lam x t _) = pretty x <+> text "=>" <+> pretty t
    pretty pi@(Pi x t _) | Pi _ _ _ <- range pi = parens (pretty x) <+> text "->" <+> pretty t
                         | otherwise = pretty x <+> text "->" <+> pretty t
    pretty (App t1 t2 _) =
        let f = if isApplicative t1 then id else parens
            g = if isAtomic t2 then id else parens
        in f (pretty t1) <+> g (pretty t2)
    pretty (Var x _) = pretty x
    pretty Type = text "Type"
    pretty Kind = text "Kind"

instance Pretty id => Pretty (TVar id a) where
    pretty (x ::: ty) = pretty x <+> char ':' <+> pretty ty
    pretty (Hole ty) =  pretty ty

    prettyList = vcat . map (\x -> pretty x <> dot)

instance Pretty id => Pretty (Rule id a) where
    pretty (lhs :--> rhs) = pretty lhs <+> text "-->" <+> pretty rhs

instance Pretty id => Pretty (TyRule id a) where
    pretty ([] :@ rule) = text "[]" <+> pretty rule
    pretty (env :@ rule) =
        encloseSep (text "[ ") (text " ]") (text ", ") (map pretty env)
        <+> pretty rule

    prettyList = vcat . map (\x -> pretty x <> dot)
