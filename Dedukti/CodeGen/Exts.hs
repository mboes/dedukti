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
import Language.Haskell.Exts.QQ
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toUpper)
import Data.List (foldl')
import qualified Data.Stream as Stream
import Prelude hiding ((*))


(*) :: Hs.SrcLoc                -- dummy source location.
(*) = Hs.SrcLoc "" 0 0

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
        where (tyname, boxname) = (varName (x .$ "ty"), varName (x .$ "box"))
              def_ty  = [dec| ((tyname)) = $(code ty) |]
              def_box = [dec| ((boxname)) = bbox $(term ty) $(Hs.var tyname) $(var x) |]
              -- Checking rules involves much of the same work as checking all
              -- declarations at top-level, so let's just call the code
              -- generation functions recursively.
              defs_rule n (env :@ lhs :--> rhs) =
                  let f (x ::: ty) rs = (emit (RS x ty []) :: Record) : rs
                      ruleCheck = let rule_box = varName (qid "rule" .$ "box")
                                  in Rec (qid "rule") 0 [[dec| ((rule_box)) = checkRule $(term lhs) $(term rhs) |]]
                      Bundle decls = coalesce $ foldr f [ruleCheck] (env_bindings env)
                      rule = varName (x .$ "rule" .$ B.pack (show n))
                      body = Hs.letE decls [hs| main |]
                  in [dec| ((rule)) = $body |]

    coalesce records = Bundle $ concatMap rec_code records ++ [main]
        where main = [dec| main = $checks |]
              checks = Hs.Do (concatMap rules records ++ map declaration records)
              declaration r = let desc = Hs.strE $ show $ pretty $ unqualify $ rec_name r
                                in Hs.qualStmt [hs| checkDeclaration $desc $(var (rec_name r .$ "box")) |]
              rules (Rec _ 0 _) = []
              rules (Rec x nr _) =
                let startmsg = Hs.strE $ "Starting rule " ++ show (pretty (unqualify x)) ++ "."
                    finishmsg = Hs.strE $ "Finished rule " ++ show (pretty (unqualify x)) ++ "."
                in [Hs.qualStmt [hs| putStrLn $startmsg |]]
                    ++ map (\n -> Hs.qualStmt $ var (x .$ "rule" .$ B.pack (show n))) [0..nr-1]
                    ++ [Hs.qualStmt [hs| putStrLn $finishmsg |]]

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
function (RS x _ []) = Hs.nameBind (*) (varName x) (constant x)
function (RS x _ rs) = Hs.sfun (*) (varName x) [] (Hs.UnGuardedRhs rhs) (Hs.binds [f])
    where n = Rule.arity (head rs)
          pats = Stream.take n variables
          occs = map Hs.var pats
          rhs = foldr (\x y -> [hs| Lam (\((x)) -> $y) |]) (Hs.metaFunction "__" occs) pats
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
    where guards = map (\(x, x') -> Hs.qualStmt $
                                    [hs| reflect (convertible 0 $(var x) $(var x')) |])
          qids = Stream.unfold (\i -> ((qid $ B.pack $ show i) .$ "fresh", i + 1)) 0

defaultClause :: Id Record -> Int -> Hs.Match
defaultClause x n =
    Hs.Match (*) (Hs.name "__") (Stream.take n (Stream.map Hs.pvar variables)) Nothing
          (Hs.UnGuardedRhs (foldl' (\e x -> [hs| App $e $x |]) (constant x) (Stream.take n (Stream.map Hs.var variables)))) Hs.noBinds

constant c = [hs| Con $(Hs.strE $ show $ pretty c) |]

pattern :: Em Env -> Em Expr -> Hs.Pat
pattern env (V x _) | x `isin` env = Hs.pvar (varName x)
pattern env expr = unapply expr (\(V x _) xs _ -> primAppsP x (map (pattern env) xs))

-- | Build a pattern matching constant.
primConP c = Hs.PParen (Hs.pApp (Hs.name "Con") [Hs.strP (show (pretty c))])
primAppP t1 t2 = Hs.PParen (Hs.pApp (Hs.name "App") [t1, t2])
primAppsP c = foldl' primAppP (primConP c)

-- | Turn an expression into object code with types erased.
code :: Em Expr -> Hs.Exp
code (B (L x) t _)      | n <- varName x = [hs| Lam (\((n)) -> $(code t)) |]
code (B (x ::: ty) t _) | n <- varName x = [hs| Pi $(code ty) (\((n)) -> $(code t)) |]
code (B (x := t1) t2 _) | n <- varName x = [hs| let ((n)) = $(code t1) in $(code t2) |]
code (A t1 t2 _)        = [hs| ap $(code t1) $(code t2) |]
code (V x _)            = var x
code Type               = [hs| Type |]

-- | Turn a term into its Haskell representation, including all types.
term :: Em Expr -> Hs.Exp
term (B (L x) t _)      | n <- varName (x .$ "box") =  [hs| TLam (\((n)) -> $(term t)) |]
term (B (x ::: ty) t _) = typedAbstraction [hs| TPi |] x ty (term t)
term (B (x := t1) t2 _) | n <- varName (x .$ "box") = [hs| TLet $(term t1) (\((n)) -> $(term t2)) |]
term (A t1 t2 _)        = [hs| TApp $(term t1) $(term t2) |]
term (V x _)            = var (x .$ "box")
term Type               = [hs| TType |]

typedAbstraction c x ty t = [hs| $c $(dom ty) (\((box)) -> $ran) |]
  where box = varName (x .$ "box")
        ran = let n = varName x
              in [hs| let ((n)) = obj $(Hs.var box) in $t |]
        dom ty = if isVariable ty
                 then term ty else [hs| sbox $(term ty) Type $(code ty) |]

varName :: Id Record -> Hs.Name
varName = Hs.name . xencode . unqualify

-- | Smart variable constructor.
var :: Id Record -> Hs.Exp
var = Hs.var . Hs.name . xencode

-- | Produce a set of variables y1, ..., yn
variables = Stream.unfold (\i -> (Hs.name $ ('y':) $ show i, i + 1)) 0

-- | Capitalize a word.
capitalize :: B.ByteString -> B.ByteString
capitalize s = case B.uncons s of
             Nothing -> B.empty
             Just (x, xs) -> toUpper x `B.cons` xs
