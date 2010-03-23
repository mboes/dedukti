-- |
-- Copyright : © 2009 CNRS - École Polytechnique - INRIA
-- License   : GPL
--
-- A code generator based on the haskell-src-exts package by Niklas Broberg.

module Dedukti.CodeGen.Exts
    (module Dedukti.CodeGen, Code) where

import Dedukti.CodeGen
import Dedukti.Core
import Dedukti.Module
import Dedukti.Pretty
import qualified Dedukti.Rule as Rule
import qualified Language.Haskell.Exts.Syntax as Hs
import qualified Language.Haskell.Exts.Build as Hs
import Language.Haskell.Exts.Pretty
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Stream as Stream
import Prelude hiding ((*))


type Em a = a (Id Record) (A Record)

type instance Id Record = Qid
type instance A  Record = Unannot

-- External view of record.
type Code = Record

-- Create a record for each declaration in the source.
data Record = Rec { rec_name    :: Qid
                  , rec_rules   :: Int -- ^ Number of rules associated with qid.
                  , rec_code    :: [Hs.Decl] }

instance CodeGen Record where
    data Bundle Record = Bundle [Hs.Decl]

    emit rs@(RS x ty rules) =
        Rec x (length rules) (function rs : def_ty : def_box : zipWith defs_rule [0..] rules)
        where def_ty  = Hs.nameBind (*) (varName (x .$ "ty")) (code ty)
              def_box = Hs.nameBind (*) (varName (x .$ "box"))
                        (primbbox (term ty) (var (x .$ "ty")) (var x))
              -- Checking rules involves much of the same work as checking all
              -- declarations at top-level, so let's just call the code
              -- generation functions recursively.
              defs_rule n (env :@ lhs :--> rhs) =
                  let rec (x ::: ty) rs = (emit (RS x ty []) :: Record) : rs
                      Bundle decls = coalesce $ foldr rec [ruleCheck] (env_bindings env)
                  in Hs.sfun (*) (varName (x .$ "rule" .$ B.pack (show n))) []
                         (Hs.UnGuardedRhs (primitiveVar "main" []))
                         (Hs.binds decls)
                      where ruleCheck = Rec (qid "rule") 0
                                            [Hs.nameBind (*) (varName (qid "rule" .$ "box"))
                                             (primitiveVar "checkRule" [term lhs, term rhs])]

    coalesce records = Bundle $ concatMap rec_code records ++ [main]
        where main = Hs.nameBind (*) (Hs.name "main") checks
              checks = Hs.Do (concatMap rules records ++ map declaration records)
              declaration rec = Hs.qualStmt (primitiveVar "checkDeclaration"
                                              [ Hs.strE $ show $ pretty $ unqualify $ rec_name rec
                                              , var (rec_name rec .$ "box") ])
              rules (Rec _ 0 _) = []
              rules (Rec x nr _) =
                  [Hs.qualStmt $ primitiveVar "putStrLn" [Hs.strE ("Starting rule " ++ show (pretty (unqualify x)) ++ ".")]] ++
                  map (\n -> Hs.qualStmt $ var (x .$ "rule" .$ B.pack (show n))) [0..nr-1] ++
                  [Hs.qualStmt $ primitiveVar "putStrLn" [Hs.strE ("Finished rule " ++ show (pretty (unqualify x)) ++ ".")]]

    serialize mod deps (Bundle decls) =
        B.pack $ prettyPrintWithMode defaultMode {layout = PPInLine} $
        Hs.Module (*) (modname mod) [] Nothing Nothing imports decls
        where imports = runtime : map (\m -> Hs.ImportDecl (*) (modname m) True False Nothing Nothing Nothing) deps
              runtime = Hs.ImportDecl (*) (Hs.ModuleName "Dedukti.Runtime") False False Nothing Nothing Nothing
              modname m = Hs.ModuleName $ B.unpack $ B.intercalate "." $ map capitalize $ toList m

-- | A similar encoding of names as the z-encoding of GHC. Non-letter
-- characters are escaped with an x.
xencode :: Qid -> String
xencode qid =
    B.unpack $
     joinQ (qid_qualifier qid) `B.append`
     -- Prepend all idents with an x to avoid clash with runtime functions.
     B.cons 'x' (enc $ fromAtom $ qid_stem qid) `B.append`
     joinS (qid_suffix qid)
        where joinQ Root = ""
              joinQ (h :. x) = joinQ h `B.append` capitalize (fromAtom x) `B.append` "."
              joinS Root = ""
              joinS (h :. x) = joinS h `B.append` "_" `B.append` fromAtom x
              enc = B.concatMap f where
                  f 'x'  = "xx"
                  f '\'' = "xq"
                  f '_'  = "xu"
                  f x | x >= '0', x <= '9' = 'x' `B.cons` B.singleton x
                      | otherwise = B.singleton x

function :: Em RuleSet -> Hs.Decl
function (RS x _ []) = Hs.nameBind (*) (varName x) (primCon x)
function (RS x _ rs) = Hs.sfun (*) (varName x) [] (Hs.UnGuardedRhs rhs) (Hs.binds [f])
    where n = Rule.arity (head rs)
          rhs = foldr primLam
                (Hs.metaFunction "__" (Stream.take n variables))
                (Stream.take n pvariables)
          f | n > 0     = Hs.FunBind (map clause rs ++ [defaultClause x n])
            | otherwise = Hs.FunBind (map clause rs)

clause :: Em TyRule -> Hs.Match
clause rule =
    let (lrule@(env :@ _ :--> rhs), constraints) = Rule.linearize qids rule
    in if null constraints
       then Hs.Match (*) (Hs.name "__") (map (pattern env) (Rule.patterns lrule))
            Nothing (Hs.UnGuardedRhs (code rhs)) Hs.noBinds
       else Hs.Match (*) (Hs.name "__") (map (pattern env) (Rule.patterns lrule))
            Nothing (Hs.GuardedRhss [Hs.GuardedRhs (*) (guards constraints) (code rhs)]) Hs.noBinds
    where guards constraints =
              map (\(x, x') -> Hs.qualStmt $
                   primitiveVar "convertible" [Hs.intE 0, var x, var x']) constraints
          qids = Stream.unfold (\i -> ((qid $ B.pack $ show i) .$ "fresh", i + 1)) 0

defaultClause :: Id Record -> Int -> Hs.Match
defaultClause x n =
    Hs.Match (*) (Hs.name "__") (Stream.take n pvariables) Nothing
          (Hs.UnGuardedRhs (primApps x (Stream.take n variables))) Hs.noBinds

pattern :: Em Env -> Em Expr -> Hs.Pat
pattern env (Var x _) | x `isin` env = pvar x
pattern env expr = case unapply expr of
                     Var x _ : xs -> primAppsP x (map (pattern env) xs)

-- | Turn an expression into object code with types erased.
code :: Em Expr -> Hs.Exp
code (Var x _)            = var x
code (Lam (x ::: ty) t _) = primLam (pvar x) (code t)
code (Lam (Hole ty) t _)  = primLam Hs.wildcard (code t)
code (Pi (x ::: ty) t _)  = primPi (code ty) (pvar x) (code t)
code (Pi (Hole ty) t _)   = primPi (code ty) Hs.wildcard (code t)
code (App t1 t2 _)        = primap (code t1) (code t2)
code Type                 = primType

-- | Turn a term into its Haskell representation, including all types.
term :: Em Expr -> Hs.Exp
term (Var x _)     = var (x .$ "box")
term (Lam b t _)   = primTLam b (term t)
term (Pi b t _)    = primTPi  b (term t)
term (App t1 t2 _) = primTApp (term t1) (primUBox (term t2) (code t2))
term Type          = primTType
term Kind          = primTKind

(*) :: Hs.SrcLoc
(*) = Hs.SrcLoc "" 0 0

varName :: Id Record -> Hs.Name
varName = Hs.name . xencode . unqualify

-- | Smart variable constructor.
var :: Id Record -> Hs.Exp
var = Hs.var . Hs.name . xencode

pvar :: Id Record -> Hs.Pat
pvar = Hs.PVar . varName

-- | Produce a set of variables y1, ..., yn
variables = Stream.unfold (\i -> (Hs.Var $ Hs.UnQual $ Hs.name $ ('y':) $ show i, i + 1)) 0

pvariables = Stream.unfold (\i -> (Hs.PVar $ Hs.name $ ('y':) $ show i, i + 1)) 0

-- Primitives

primitiveVar s [] = Hs.var $ Hs.name s
primitiveVar s xs = Hs.Paren $ Hs.metaFunction s xs

primitiveCon s [] = Hs.Con $ Hs.UnQual $ Hs.name s
primitiveCon s xs = Hs.Paren $ Hs.appFun (Hs.Con $ Hs.UnQual $ Hs.name s) xs

primap  t1 t2 = primitiveVar "ap"  [t1, t2]
primApp t1 t2 = primitiveCon "App" [t1, t2]
primCon c     = primitiveCon "Con" [Hs.strE $ show $ pretty c]
primType      = primitiveCon "Type" []

primLam pat t = primitiveCon "Lam" [Hs.Paren (Hs.Lambda (*) [pat] t)]
primPi  dom pat range = primitiveCon "Pi" [dom, Hs.Paren (Hs.Lambda (*) [pat] range)]

primApps c = foldl' primApp (primCon c)

typedAbstraction c b t =
    let (pat, ty, ran) =
            case b of
              x ::: ty -> ( pvar (x .$ "box")
                          , ty
                          , Hs.Let (Hs.binds [Hs.nameBind (*) (varName x) (primobj (var (x .$ "box")))]) t )
              Hole ty  -> (Hs.wildcard, ty, t)
        dom = if isVariable ty
              then term ty else primsbox (term ty) primType (code ty)
    in primitiveCon c [dom, Hs.Paren (Hs.Lambda (*) [pat] ran)]

primTLam       = typedAbstraction "TLam"
primTPi        = typedAbstraction "TPi"
primTApp t1 t2 = primitiveCon "TApp" [t1, t2]
primTType      = primitiveCon "TType" []
primTKind      = primitiveCon "TKind" []

primUBox ty obj_code         = primitiveCon "UBox" [ty, obj_code]
primbbox ty ty_code obj_code = primitiveVar "bbox" [ty, ty_code, obj_code]
primsbox ty ty_code obj_code = primitiveVar "sbox" [ty, ty_code, obj_code]

primobj t = primitiveVar "obj" [t]

-- | Build a pattern matching a constant.
primConP c = Hs.PParen $ Hs.PApp (Hs.UnQual $ Hs.name "Con") [Hs.PLit (Hs.String (show (pretty c)))]
primAppP t1 t2 = Hs.PParen $ Hs.PApp (Hs.UnQual $ Hs.name "App") [t1, t2]
primAppsP c = foldl' primAppP (primConP c)

-- | Capitalize a word.
capitalize :: B.ByteString -> B.ByteString
capitalize s = case B.uncons s of
             Nothing -> B.empty
             Just (x, xs) -> toUpper x `B.cons` xs
