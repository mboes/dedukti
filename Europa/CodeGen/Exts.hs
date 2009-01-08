module Europa.CodeGen.Exts
    (module Europa.CodeGen, Code) where

import Europa.CodeGen
import Europa.Core
import Europa.Module
import qualified Europa.Rule as Rule
import qualified Language.Haskell.Exts.Syntax as Hs
import Language.Haskell.Exts.Pretty
import qualified Data.Map as Map (member)
import qualified Data.ByteString as B
import Data.Char (ord, toUpper)
import Data.List (intercalate)


type Em a = a String Unannot

-- External view of record.
type Code = Record

data Record = Rec { rec_name    :: String
                  , rec_type    :: Em Expr
                  , rec_variant :: Variant
                  , rec_code    :: [Hs.Match] }

(!) :: Hs.SrcLoc
(!) = Hs.SrcLoc "" 0 0

varName :: String -> Hs.Name
varName s = Hs.Ident ('x':s)

conName :: String -> Hs.Name
conName s = Hs.Ident ('X':s)

-- | Smart variable constructor.
var :: String -> Hs.Exp
var = Hs.Var . Hs.UnQual . varName

pvar :: String -> Hs.Pat
pvar = Hs.PVar . varName

-- | Produce a set of variables x1, ..., xn
variables n = map (var . show) [1..n]

pvariables n = map (pvar . show) [1..n]

-- | Smart constructor constructor.
con :: String -> Hs.Exp
con = Hs.Con . Hs.UnQual . conName

application :: [Hs.Exp] -> Hs.Exp
application = foldl1 Hs.App

-- | Build a constructor pattern.
papplication :: String -> [Hs.Pat] -> Hs.Pat
papplication c xs = Hs.PApp (Hs.UnQual (conName c)) xs

-- Primitives

primitiveVar s [] = Hs.Var $ Hs.UnQual $ Hs.Ident s
primitiveVar s xs = Hs.Paren $ application $ (Hs.Var $ Hs.UnQual $ Hs.Ident s) : xs

primitiveCon s [] = Hs.Con $ Hs.UnQual $ Hs.Ident s
primitiveCon s xs = Hs.Paren $ application $ (Hs.Con $ Hs.UnQual $ Hs.Ident s) : xs

primitiveAbstraction c Nothing t =
    primitiveCon c [Hs.Paren (Hs.Lambda (!) [Hs.PWildCard] t)]
primitiveAbstraction c (Just x) t =
    primitiveCon c [Hs.Paren (Hs.Lambda (!) [pvar x] t)]

primap  t1 t2 = primitiveVar "ap"  [t1, t2]
primApp t1 t2 = primitiveCon "App" [t1, t2]
primLam       = primitiveAbstraction "Lam"
primCon c     = primitiveCon "Con" [Hs.Lit (Hs.String ('X':c))]

primApps c = foldl primApp (primCon c)

primTLam       = primitiveAbstraction "TLam"
primTPi        = primitiveAbstraction "TPi"
primTApp t1 t2 = primitiveCon "TApp" [t1, t2]
primTType      = primitiveCon "TType" []
primTKind      = primitiveCon "TKind" []
primTBox x     = primitiveCon "TBox" [var (x ++ "_ty")]

primtypeOf t = primitiveVar "typeOf" [t]

-- | Build a pattern matching a constant.
primConP c = Hs.PApp (Hs.UnQual $ Hs.Ident "Con") [Hs.PLit (Hs.String c)]
primAppP t1 t2 = Hs.PApp (Hs.UnQual $ Hs.Ident "App") [t1, t2]
primAppsP c = foldl primAppP (primConP c)

-- | Upcase a word.
upcase :: String -> String
upcase "" = ""
upcase (x:xs) = toUpper x : xs

packString :: String -> B.ByteString
packString = B.pack . map c2w where
    c2w = fromIntegral . ord

instance CodeGen Record where
    type Id Record     = String
    type A Record      = Unannot
    type Bundle Record = [Hs.Decl]

    interface = error "Unimplemented."

    coalesce records = decls ++ [main]
        where decls = map (Hs.FunBind . rec_code) records
              main = Hs.FunBind [Hs.Match (!) (Hs.Ident "main") []
                                       (Hs.UnGuardedRhs checks) (Hs.BDecls [])]
              checks = primitiveVar "runChecks"
                       [Hs.List $ map (var . (++ "_check") . rec_name) objects]
              objects = filter ((== VtObject) . rec_variant) records

    serialize _ (Module mod) decls =
        packString $ prettyPrint $
        Hs.Module (!) modname [] Nothing Nothing imports decls
        where imports = [ Hs.ImportDecl (!) (Hs.ModuleName "Europa.Runtime")
                                        False False Nothing Nothing ]
              modname = Hs.ModuleName $ intercalate "." $ map upcase (toList mod)

    -- Assume all rules have same head constant and same arity.
    emit v@VtObject (RS x ty rules)
        | n == 0 = Rec x ty v clauses
        | otherwise = Rec x ty v (clauses ++ [defclause])
        where n = if null rules then 0 else Rule.arity (head rules)
              -- default clause
              defclause =
                  Hs.Match (!) (varName x) (pvariables n)
                        (Hs.UnGuardedRhs (primApps x (variables n)))
                        (Hs.BDecls [])
              clauses = map (clause x) rules

    emit v@VtType (RS x ty _) =
        Rec x ty v [Hs.Match (!) (varName (x ++ "_ty")) []
                           (Hs.UnGuardedRhs (typed primap ty))
                           (Hs.BDecls [])]

    emit v@VtSort (RS x ty _) =
        Rec x ty v [Hs.Match (!) (varName (x ++ "_check")) []
                           (Hs.UnGuardedRhs (primtypeOf (check ty)))
                           (Hs.BDecls [])]

clause :: String -> Em TyRule -> Hs.Match
clause x rule@(env :@ (lhs :--> rhs)) =
    Hs.Match (!) (varName x) (map (pattern env) (Rule.patterns rule))
--      (Hs.UnGuardedRhs (untyped primap rhs)) (Hs.BDecls [])
      (Hs.UnGuardedRhs (typed primTApp rhs)) (Hs.BDecls [])

pattern :: Em Env -> Em Expr -> Hs.Pat
pattern env (Var x _) | Map.member x env = pvar x
pattern env expr = case unapply expr of
                     Var x _ : xs -> primAppsP x (map (pattern env) xs)

-- | Turn an expression into object code with types erased.
untyped :: Em Expr -> Hs.Exp
untyped (Var x _)            = var x
untyped (Lam (x ::: ty) t _) = primLam (Just x) (untyped t)
untyped (Lam (Hole ty) t _)  = primLam Nothing (untyped t)
untyped (Pi b t _)           = untyped (Lam b t undefined)
untyped (App t1 t2 _)        = primApp (untyped t1) (untyped t2)
untyped Type                 = primCon "Type"
untyped Kind                 = primCon "Kind"

-- | Turn a term into its Haskell representation, including all types.
typed :: Em Expr -> Hs.Exp
typed (Var x _)              = var x
typed (Lam b@(x ::: ty) t _) = primTLam (Just x) (typed t)
typed (Lam (Hole ty) t a)    = primTLam Nothing (typed t)
typed (Pi b@(x ::: ty) t _)  = primTPi (Just x) (typed t)
typed (Pi (Hole ty) t a)     = primTPi Nothing (typed t)
typed (App t1 t2 _)          = primTApp (typed t1) (typed t2)
typed Type                   = primTType
typed Kind                   = primTKind

check :: Em Expr -> Hs.Exp
check (Var x _)              = primTBox x
check (Lam b@(x ::: ty) t _) = primTLam (Just x) (primtypeOf (check t))
check (Lam (Hole ty) t a)    = primTLam Nothing (primtypeOf (check t))
check (Pi b@(x ::: ty) t _)  = primTPi (Just x) (primtypeOf (check t))
check (Pi (Hole ty) t a)     = primTPi Nothing (primtypeOf (check t))
check (App t1 t2 _)          = primApp (check t1) (check t2)
check Type                   = primTType
check Kind                   = primTKind
