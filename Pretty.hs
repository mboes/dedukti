module Pretty where

import Core
import Text.PrettyPrint.Leijen


instance Pretty id => Pretty (Expr id a) where
    pretty (Lam x t _) = parens (pretty x <+> text "=>" <> softline <> pretty t)
    pretty pi@(Pi x t _) | Pi _ _ _ <- range pi = parens (pretty x) <+> text "->" <+> pretty t
                         | otherwise = pretty x <+> text "->" <+> pretty t
    pretty (App t1 t2 _) = pretty t1 <+> pretty t2
    pretty (Var x _) = pretty x
    pretty Type = text "Type"
    pretty Kind = text "Kind"

instance Pretty id => Pretty (TVar id a) where
    pretty (x ::: ty) = pretty x <+> char ':' <+> pretty ty
    pretty (Hole ty) =  pretty ty

    prettyList xs = vcat $ map (\x -> pretty x <> dot) xs

instance Pretty id => Pretty (Rule id a) where
    pretty (lhs :--> rhs) = pretty lhs <+> text "-->" <+> pretty rhs

instance Pretty id => Pretty (TyRule id a) where
    pretty (env :@ rule) =
        encloseSep (char '[') (char ']') (text ", ") (map pretty env)
        <+> pretty rule
