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
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad(forM)
import qualified Data.Map as Map

import Debug.Trace

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

-- TODO source filename
prolog :: String -> String
prolog = ["target datalayout = \"e-m:e-i64:64-f80:128-n8:16:32:64-S128",
          "target triple = \"x86_64-pc-linux-gnu",
          "@.str = private unnamed_addr constant [4 x i8] c\"%d\0A\00\", align 1"
         ]
main_begin = ["define i32 @main() #0 {"]
main_end = ["}"]

epilog = ["attributes #0 = { noinline nounwind optnone uwtable \"correctly-rounded-divide-sqrt-fp-math\"=\"false\" \"disable-tail-calls\"=\"false\" \"less-precise-fpmad\"=\"false\" \"no-frame-pointer-elim\"=\"true\" \"no-frame-pointer-elim-non-leaf\" "no-infs-fp-math\"=\"false\" \"no-jump-tables\"=\"false\" \"no-nans-fp-math\"=\"false\" \"no-signed-zeros-fp-math\"=\"false\" \"no-trapping-math\"=\"false\" \"stack-protector-buffer-size\"=\"8\" \"target-cpu\"=\"x86-64\" \"target-features\"=\"+fxsr,+mmx,+sse,+sse2,+x87\" \"unsafe-fp-math\"=\"false\" \"use-soft-float\"=\"false\" }",
          "!llvm.module.flags = !{!0}",
          "!llvm.ident = !{!1}",
          "!0 = !{i32 1, !\"wchar_size\", i32 4}",
          "!1 = !{!\"clang version 6.0.0-1ubuntu2 (tags/RELEASE_600/final)\"}"
]


buildPrintable :: T.Text -> T.Text
buildPrintable t = buildText [T.pack print_begin,
                              t,
                              T.pack print_end]

buildConstructor :: T.Text
buildConstructor = buildLines default_constructor

buildMainBegin :: T.Text
buildMainBegin = buildLines main_begin

buildMainEnd :: T.Text
buildMainEnd = buildLines main_end

buildText :: [T.Text] -> T.Text
buildText ts = T.intercalate (T.pack "\n") ts

buildLines :: [String] -> T.Text
buildLines xs = T.intercalate (T.pack "\n") (map T.pack xs) 



-- num variable to be used (numering from 1,
-- because main is static and takes one argument)
-- and map from variable name to its number
type Val = (Integer, Map.Map String Integer)
type SState a = State Val a

val0 :: Val
val0 = (1, Map.empty)

-- it must exist in val
getVariableNum :: String -> Val -> Integer 
getVariableNum x (_, m) = m Map.! x

modifyAssignVariable :: String -> Val -> Val
modifyAssignVariable x (num, m) = case Map.lookup x m of
  Nothing -> (num + 1, Map.insert x num m)
  Just _ -> (num, m)


computeBinaryOpIR :: Exp -> Exp -> Map.Map Exp Integer -> T.Text -> SState T.Text
computeBinaryOpIR e1 e2 m opBytecode = do
  t1 <- computeExpIR e1 m
  t2 <- computeExpIR e2 m
  if (m Map.! e1) > (m Map.! e2)
    then return $ buildText [t1, t2, opBytecode]
    else return $ buildText [t2, t1, opBytecode]

    
computeExpIR :: Exp -> Map.Map Exp Integer -> SState T.Text
computeExpIR e m = do
  case e of
    ExpAdd e1 e2 -> computeBinaryOpIR e1 e2 m (T.pack "iadd")
    ExpSub e1 e2 -> computeBinaryOpIR e1 e2 m (T.pack "isub")
    ExpMul e1 e2 -> computeBinaryOpIR e1 e2 m (T.pack "imul")
    ExpDiv e1 e2 -> computeBinaryOpIR e1 e2 m (T.pack "idiv")
    ExpLit n -> return $ T.pack $ (push n) ++ (show n) where
      push n
        | n <= 255 = "bipush "
        | n <= 65535 = "sipush "
        | otherwise = "ldc "
    ExpVar (Ident x) -> do
      m <- gets snd
      case Map.lookup x m of
        Nothing -> error "Usage of not defined variable"
        Just num -> return $ T.pack $ "iload " ++ (show num)

computeStmtIR :: Stmt -> SState T.Text
computeStmtIR stmt = do
  case stmt of
    SAss (Ident x) e -> do
      t1 <- computeExpIR e
      return $ buildText [] 
    SExp e -> do
      let m = computeStackMap e
      t1 <- computeExpIR e m
      return $ buildPrintable t1


computeProgramIR :: Program -> SState T.Text
computeProgramIR (Prog stmts) = do
  ts <- forM stmts computeStmtIR
  return $ buildText ts

-- returns content and number of used variables (for limitLocals)
buildMainContentIR :: Program -> Val -> (T.Text, Integer)
buildMainContentIR p v = (text, fromIntegral $ Map.size $ snd state) where
  (text, state) = runState (computeProgramIR p) v

-- limits for locals and stack increased by 1 because of 'getstatic' usage
-- and string argument in main function
buildMain :: Program -> T.Text
buildMain prog = buildText [buildMainBegin,
                                   limitStack $ 1 + (computeStackLimit prog),
                                   limitLocals $ 1 + locals,
                                   text,
                                   buildMainEnd] where
  (text, locals) = buildMainContentIR prog val0

buildIR :: Program -> T.Text
buildIR prog = buildText [buildProlog "PrzykladowaKlasa",
                                  buildConstructor,
                                  buildMain prog]



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
