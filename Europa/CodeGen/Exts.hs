-- |
-- Copyright : (c) 2009 INRIA
-- License   : GPL
--
-- A code generator based on the haskell-src-exts package by Niklas Broberg.

module Europa.CodeGen.Exts
    (module Europa.CodeGen, Code) where

import Europa.CodeGen
import Europa.Core
import Europa.Module
import Europa.Pretty
import qualified Europa.Rule as Rule
import qualified Language.Haskell.Exts.Syntax as Hs
import Language.Haskell.Exts.Pretty
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (ord, toUpper)
import Data.List (intercalate, concatMap)
import qualified Data.Stream as Stream


type Em a = a (Id Record) (A Record)

-- External view of record.
type Code = Record

-- Create a record for each declaration in the source.
data Record = Rec { rec_name    :: Qid
                  , rec_rules   :: Int -- ^ Number of rules associated with
                                       -- qid.
                  , rec_code    :: [Hs.Decl] }

instance CodeGen Record where
    type Id Record     = Qid
    type A Record      = Unannot
    data Bundle Record = Bundle [Hs.Decl]

    emit rs@(RS x ty rules) =
        Rec x (length rules) (function rs : def_ty : def_box : zipWith defs_rule [0..] rules)
        where def_ty  = value (x .$ "ty") (code ty)
              def_box = value (x .$ "box")
                                     (primbbox (term ty) (var (x .$ "ty")) (var x))
              -- Checking rules involves much of the same work as checking all
              -- declarations at top-level, so let's just call the code
              -- generation functions recursively.
              defs_rule n (env :@ lhs :--> rhs) =
                  let rec x ty rs = (emit (RS x ty []) :: Record) : rs
                      Bundle decls = coalesce $ Map.foldWithKey rec [ruleCheck] env
                  in  Hs.FunBind [Hs.Match (!) (varName (x .$ "rule" .$ B.pack (show n)))
                                  []
                                  Nothing
                                  (Hs.UnGuardedRhs (primitiveVar "main" []))
                                  (Hs.BDecls decls)]
                      where ruleCheck = Rec (qid "rule") 0
                                            [value (qid "rule" .$ "box")
                                             (primitiveVar "checkRule" [var (x .$ "ty"), term lhs, term rhs])]

    coalesce records = Bundle $ concatMap rec_code records ++ [main]
        where main = Hs.FunBind [Hs.Match (!) (Hs.Ident "main") []
                                       Nothing (Hs.UnGuardedRhs checks) (Hs.BDecls [])]
              checks = Hs.Do (concatMap rules records ++ map declaration records)
              declaration rec = Hs.Qualifier (primitiveVar "checkDeclaration"
                                              [ Hs.Lit $ Hs.String $ show $ pretty $ rec_name rec
                                              , var (rec_name rec .$ "box") ])
              rules (Rec _ 0 _) = []
              rules (Rec x nr _) =
                  [Hs.Qualifier $ primitiveVar "putStrLn" [Hs.Lit $ Hs.String ("Starting rule " ++ show (pretty x))]] ++
                  map (\n -> Hs.Qualifier $ var (x .$ "rule" .$ B.pack (show n))) [0..nr-1] ++
                  [Hs.Qualifier $ primitiveVar "putStrLn" [Hs.Lit $ Hs.String ("Finished rule " ++ show (pretty x))]]

    serialize mod deps (Bundle decls) =
        B.pack $ prettyPrint $
        Hs.Module (!) (modname mod) [] Nothing Nothing imports decls
        where imports = runtime : map (\m -> Hs.ImportDecl (!) (modname m) True False Nothing Nothing) deps
              runtime = Hs.ImportDecl (!) (Hs.ModuleName "Europa.Runtime") False False Nothing Nothing
              modname (Module m) = Hs.ModuleName $ B.unpack $ B.intercalate "." $
                                   map upcase $ toList m

    interface = error "Unimplemented."

-- | A similar encoding of names as the z-encoding of GHC. Non-letter
-- characters are escaped with an x.
xencode :: Qid -> String
xencode qid =
    B.unpack $
     joinQ (qid_qualifier qid) `B.append`
     -- Prepend all idents with an x to avoid clash with runtime functions.
     B.cons 'x' (enc (qid_stem qid)) `B.append`
     joinS (qid_suffix qid)
        where joinQ Root = ""
              joinQ (h :. x) = joinQ h `B.append` upcase x `B.append` "."
              joinS Root = ""
              joinS (h :. x) = joinS h `B.append` "_" `B.append` x
              enc = B.concatMap f where
                  f 'x'  = "xx"
                  f '\'' = "xq"
                  f '_'  = "xu"
                  f x | x >= '0', x <= '9' = 'x' `B.cons` B.singleton x
                      | otherwise = B.singleton x

function :: Em RuleSet -> Hs.Decl
function (RS x _ []) =
    Hs.FunBind [Hs.Match (!) (varName x) [] Nothing (Hs.UnGuardedRhs (primCon x)) (Hs.BDecls [])]
function (RS x _ rs) =
    Hs.FunBind [Hs.Match (!) (varName x) [] Nothing (Hs.UnGuardedRhs rhs) (Hs.BDecls [f])]
    where n = Rule.arity (head rs)
          rhs = foldr primLam
                (application (Hs.Var (Hs.UnQual (Hs.Ident "__")) : Stream.take n variables))
                (Stream.take n pvariables)
          f | n > 0     = Hs.FunBind (map clause rs ++ [defaultClause x n])
            | otherwise = Hs.FunBind (map clause rs)

clause :: Em TyRule -> Hs.Match
clause rule =
    let (lrule@(env :@ lhs :--> rhs), constraints) = Rule.linearize qids rule
    in if null constraints
       then Hs.Match (!) (Hs.Ident "__") (map (pattern env) (Rule.patterns lrule))
            Nothing (Hs.UnGuardedRhs (code rhs)) (Hs.BDecls [])
       else Hs.Match (!) (Hs.Ident "__") (map (pattern env) (Rule.patterns lrule))
            Nothing (Hs.GuardedRhss [Hs.GuardedRhs (!) (guards constraints) (code rhs)]) (Hs.BDecls [])
    where guards constraints =
              map (\(x, x') -> Hs.Qualifier $
                   primitiveVar "convertible" [Hs.Lit (Hs.Int 0), var x, var x']) constraints
          qids = Stream.unfold (\i -> ((qid $ B.pack $ show i) .$ "fresh", i + 1)) 0

defaultClause :: Id Record -> Int -> Hs.Match
defaultClause x n =
    Hs.Match (!) (Hs.Ident "__") (Stream.take n pvariables) Nothing
          (Hs.UnGuardedRhs (primApps x (Stream.take n variables))) (Hs.BDecls [])

value :: Id Record -> Hs.Exp -> Hs.Decl
value x rhs =
    Hs.FunBind [Hs.Match (!) (varName x) [] Nothing (Hs.UnGuardedRhs rhs) (Hs.BDecls [])]


pattern :: Em Env -> Em Expr -> Hs.Pat
pattern env (Var x _) | x `Map.member` env = pvar x
pattern env expr = case unapply expr of
                     Var x _ : xs -> primAppsP x (map (pattern env) xs)

-- | Turn an expression into object code with types erased.
code :: Em Expr -> Hs.Exp
code (Var x _)            = var x
code (Lam (x ::: ty) t _) = primLam (pvar x) (code t)
code (Lam (Hole ty) t _)  = primLam Hs.PWildCard (code t)
code (Pi (x ::: ty) t _)  = primPi (code ty) (pvar x) (code t)
code (Pi (Hole ty) t _)   = primPi (code ty) Hs.PWildCard (code t)
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

(!) :: Hs.SrcLoc
(!) = Hs.SrcLoc "" 0 0

varName :: Id Record -> Hs.Name
varName x = Hs.Ident $ xencode x

conName :: Id Record -> Hs.Name
conName x = Hs.Ident $ xencode x

-- | Smart variable constructor.
var :: Id Record -> Hs.Exp
var = Hs.Var . Hs.UnQual . varName

pvar :: Id Record -> Hs.Pat
pvar = Hs.PVar . varName

-- | Produce a set of variables y1, ..., yn
variables =
    Stream.unfold (\i -> (Hs.Var $ Hs.UnQual $ Hs.Ident $ ('y':) $ show i, i + 1)) 0

pvariables =
    Stream.unfold (\i -> (Hs.PVar $ Hs.Ident $ ('y':) $ show i, i + 1)) 0

application :: [Hs.Exp] -> Hs.Exp
application = foldl1 Hs.App

-- Primitives

primitiveVar s [] = Hs.Var $ Hs.UnQual $ Hs.Ident s
primitiveVar s xs = Hs.Paren $ application $ (Hs.Var $ Hs.UnQual $ Hs.Ident s) : xs

primitiveCon s [] = Hs.Con $ Hs.UnQual $ Hs.Ident s
primitiveCon s xs = Hs.Paren $ application $ (Hs.Con $ Hs.UnQual $ Hs.Ident s) : xs

primap  t1 t2 = primitiveVar "ap"  [t1, t2]
primApp t1 t2 = primitiveCon "App" [t1, t2]
primCon c     = primitiveCon "Con" [Hs.Lit (Hs.String (show (pretty c)))]
primType      = primitiveCon "Type" []
primKind      = primitiveCon "Kind" []

primLam pat t = primitiveCon "Lam" [Hs.Paren (Hs.Lambda (!) [pat] t)]
primPi  dom pat range = primitiveCon "Pi" [dom, Hs.Paren (Hs.Lambda (!) [pat] range)]

primApps c = foldl primApp (primCon c)

typedAbstraction c b t =
    let (pat, ty, ran) =
            case b of
              x ::: ty -> ( pvar (x .$ "box")
                          , ty
                          , Hs.Let (Hs.BDecls [value x (primobj (var (x .$ "box")))]) t )
              Hole ty  -> (Hs.PWildCard, ty, t)
        dom = if isVariable ty
              then term ty else primsbox (term ty) primType (code ty)
    in primitiveCon c [dom, Hs.Paren (Hs.Lambda (!) [pat] ran)]

primTLam       = typedAbstraction "TLam"
primTPi        = typedAbstraction "TPi"
primTApp t1 t2 = primitiveCon "TApp" [t1, t2]
primTType      = primitiveCon "TType" []
primTKind      = primitiveCon "TKind" []

primBox  ty_code obj_code    = primitiveCon "Box" [ty_code, obj_code]
primUBox ty obj_code         = primitiveCon "UBox" [ty, obj_code]
primbbox ty ty_code obj_code = primitiveVar "bbox" [ty, ty_code, obj_code]
primsbox ty ty_code obj_code = primitiveVar "sbox" [ty, ty_code, obj_code]

primobj t = primitiveVar "obj" [t]

primtypeOf t = primitiveVar "typeOf" [t]

-- | Build a pattern matching a constant.
primConP c = Hs.PParen $ Hs.PApp (Hs.UnQual $ Hs.Ident "Con") [Hs.PLit (Hs.String (show (pretty c)))]
primAppP t1 t2 = Hs.PParen $ Hs.PApp (Hs.UnQual $ Hs.Ident "App") [t1, t2]
primAppsP c = foldl primAppP (primConP c)

-- | Upcase a word.
upcase :: B.ByteString -> B.ByteString
upcase s = case B.uncons s of
             Nothing -> ""
             Just (x, xs) -> toUpper x `B.cons` xs
