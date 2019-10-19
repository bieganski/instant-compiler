import AbsInstant

import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad(forM)
import qualified Data.Map as Map

default_superclass = "java/lang/Object"
main_signature = ".method public static main([Ljava/lang/String;)V"
default_constructor = [".method public <init>()V",
                       "  aload_0",
                       "  invokenonvirtual java/lang/Object/<init>()V",
                       "  return",
                       ".end method"]

main_begin = [".method public static main([Ljava/lang/String;)V"]
            
main_end = ["return",
            ".end method"]

print_begin = "getstatic java/lang/System/out Ljava/io/PrintStream;"
print_end = "invokevirtual java/io/PrintStream/println(I)V"

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

buildProlog :: String -> T.Text
buildProlog classname = buildLines [cls, super] where
  cls = ".class " ++ classname 
  super = ".super " ++ default_superclass


limitStack :: Integer -> T.Text
limitStack n = T.pack $ ".limit stack " ++ (show n)

limitLocals :: Integer -> T.Text
limitLocals n = T.pack $ ".limit locals " ++ (show n)


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


buildMain :: Program -> T.Text
buildMain prog = buildText [buildMainBegin,
                                   limitStack 1000,
                                   limitLocals 1000,
                                   T.pack print_begin,
                                   buildMainContentIR prog val0,
                                   buildMainEnd]

buildIR :: Program -> T.Text
buildIR prog = buildText [buildProlog "PrzykladowaKlasa",
                                  buildConstructor,
                                  buildMain prog]


emptyProgram :: Program
emptyProgram = Prog []

computeExpIR :: Exp -> SState T.Text
computeExpIR e = return $ T.pack "lol"


computeStmtIR :: Stmt -> SState T.Text
computeStmtIR stmt = do
  case stmt of
    SAss (Ident x) e -> do
      modify (modifyAssignVariable x)
      t1 <- computeExpIR e
      val <- get
      let xnum = getVariableNum x val 
      return $ buildText [t1, T.pack $ "istore " ++ (show xnum)] 
    SExp e -> do
      t1 <- computeExpIR e
      return $ buildPrintable t1


computeProgramIR :: Program -> SState T.Text
computeProgramIR (Prog stmts) = do
  ts <- forM stmts computeStmtIR
  return $ buildText ts

buildMainContentIR :: Program -> Val -> T.Text
buildMainContentIR p v = evalState (computeProgramIR p) v
  
main :: IO ()
main = do
  putStrLn $ T.unpack $ buildIR emptyProgram
