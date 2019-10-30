import AbsInstant
import ParInstant
import SkelInstant
import PrintInstant
import SkelInstant
import LexInstant
import ErrM

import System.IO (hGetContents)
import System.Environment (getArgs)
import qualified Data.Text as T
import Text.Printf(printf)
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad(forM)
import qualified Data.Map as Map

import Debug.Trace

-- bnfc stuff
type ParseFun a = [Token] -> Err a
myLLexer = myLexer
type Verbosity = Int

-- TODO source filename
prolog = ["source_filename = \"test.c\"",
          "target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128\"",
          "target triple = \"x86_64-pc-linux-gnu\"\n",
          "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n"]
         
main_begin = ["define i32 @main() #0 {"]
main_end = ["ret i32 0", "}"]

printf_call = "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %%%d)"

epilog = ["attributes #0 = { noinline nounwind optnone uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }",
          "attributes #1 = { \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" \"no-infs-fp-math\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }",
          "!llvm.module.flags = !{!0}",
          "!llvm.ident = !{!1}\n",
          "!0 = !{i32 1, !\"wchar_size\", i32 4}",
          "!1 = !{!\"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)\"}"]


buildPrintable :: Id -> T.Text
buildPrintable id = T.pack $ printf printf_call id 

buildMainBegin :: T.Text
buildMainBegin = buildLines main_begin

buildMainEnd :: T.Text
buildMainEnd = buildLines main_end

buildText :: [T.Text] -> T.Text
buildText ts = T.intercalate (T.pack "\n") ts

buildLines :: [String] -> T.Text
buildLines xs = T.intercalate (T.pack "\n") (map T.pack xs) 


-- num free variable to be used (numering from 1)
-- and map from variable name to its number and value
type Id = Integer
type SState = Map.Map String Id
type SStateM = State (Id, SState)

s0 :: (Id, SState)
s0 = (1, Map.empty)

evalBinOp :: Exp -> Exp -> SStateM (T.Text, (Id, Id))
evalBinOp e1 e2 = do
  (t1, id1) <- evalExp e1
  (t2, id2) <- evalExp e2
  return (buildText [t1, t2], (id1, id2))

newId :: SStateM Id
newId = do
  (id, _) <- get
  modify ((+1) . fst)
  return id

getBinArgExpPrintf :: Exp -> String
getBinArgExpPrintf e = case e of
  ExpAdd _ _ -> "%%%d = add nsw %%%d, %%%d"
  ExpMul _ _ -> "%%%d = mul nsw %%%d, %%%d"
  ExpSub _ _ -> "%%%d = sub nsw %%%d, %%%d"
  ExpDiv _ _ -> "%%%d = div nsw %%%d, %%%d"
  otherwise -> error "internal - getBinArgExpPrintf"


-- besides IR text code it returns id of virtual register,
-- that holds evaluation result
evalExp :: Exp -> SStateM (T.Text, Id)
evalExp e = case e of
  ExpVar x -> do
    resId <- newId
    (_, s) <- get
    let xMemReg = getVariableNum x s
    return (T.pack $ printf "%%%d = load i32, i32* %%%d, align 4" newId xMemReg,
            newId)
  ExpLit n -> do
    tmpMemId <- newId
    valRegId <- newId
    let resLines = [printf "%%%d = alloca i32, align4" tmpMemId,
                    printf "store i32 %d, i32* %%%d, align 4" n tmpMemId,
                    printf "%%%d = load i32, i32* %%%d, align 4" valRegId tmpMemId]
    return (buildLines resLines, newId)
  ExpAdd e1 e2 -> do
    (t, (id1, id2)) <- evalBinOp e1 e2
    resId <- newId
    return (resText, resId) where
      resText = buildText [t, T.pack $ printf (getBinArgExpPrintf e) resId, id1, id2]


modifyAssignVariable :: String -> (Id, SState) -> (Id, SState)
modifyAssignVariable x (num, m) = case Map.lookup x m of
    Nothing -> (num + 1, Map.insert x num m)
    Just _ -> (num, m)

variableUsed :: String -> SStateM Bool
variableUsed x = do
  (_, m) <- get
  case Map.lookup x m of
    Nothing -> return False
    Just _ -> return True

computeStmtIR :: Stmt -> SStateM T.Text
computeStmtIR stmt = do
  case stmt of
    SAss (Ident x) e -> do
      used <- variableUsed x
      (t, id) <- evalExp e
      modify (modifyAssignVariable x)
      (_, m) <- get
      let xMemAddr = m Map.! x
      let tStore = printf "store i32 %%%d, i32* %%%d, align 4" id xMemAddr
      let resTextL = [t, tStore]
      case used of
        True -> return $ buildText resTextL
        False -> return $ buildText (r:resTextL) where
          r = T.pack $ printf "%%%d = alloca i32, align 4" xMemAddr
    SExp e -> do
      (t, id) <- evalExp e
      return $ buildText [t, buildPrintable id]


computeProgramIR :: Program -> SStateM T.Text
computeProgramIR (Prog stmts) = do
  ts <- forM stmts computeStmtIR
  return $ buildText ts


buildMainContentIR :: Program -> (Id, SState) -> T.Text
buildMainContentIR p s = evalState (computeProgramIR p) s

-- limits for locals and stack increased by 1 because of 'getstatic' usage
-- and string argument in main function
-- buildMain :: Program -> T.Text
-- buildMain

buildIR :: Program -> T.Text
buildIR prog = buildText [buildLines prolog,
                         buildLines main_begin,
                         buildLines main_end,
                         buildLines epilog]



runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v

run :: Verbosity -> String -> IO ()
run v s = do
  let ts = pProgram $ myLLexer s
  case ts of
           Bad s    -> putStrLn "\nParse failed...\n"
           Ok  tree -> do
             let ir = T.unpack $ buildIR tree
             putStrLn ir

main :: IO ()
main = do
  args <- getArgs
  case args of
    [progPath] -> do
      runFile 0 progPath
    _ -> error "args error!"
