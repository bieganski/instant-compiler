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

printf_call = "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %d)"

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
type Val = Integer
type SState = Map.Map String (Id, Val)
type SStateM = State (Id, SState)

s0 :: (Id, SState)
s0 = (1, Map.empty)

-- it must exist
getVariableNum :: String -> SState -> Integer 
getVariableNum x m = fst (m Map.! x)


-- TODO o to
modifyAssignVariable :: String -> (Id, SState) -> (Id, SState)
modifyAssignVariable x (num, m) = case Map.lookup x m of
  Nothing -> (num + 1, Map.insert x (num, 0) m)
  Just _ -> (num, m)


computeStmtIR :: Stmt -> SStateM T.Text
computeStmtIR stmt = do
  case stmt of
    SAss (Ident x) e -> do
      return $ buildText [] 
    SExp e -> do
      return $ buildPrintable 2137


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
