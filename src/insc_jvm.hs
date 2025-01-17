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
import System.FilePath
import System.Process 

-- bcnf stuff
type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int




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

stackNeeded :: Integer -> Integer -> Integer
stackNeeded e1stack e2stack = if
  e1stack == e2stack
  then e1stack + 1
  else max e1stack e2stack

computeParentDepth :: Exp -> Exp -> Map.Map Exp Integer -> Integer
computeParentDepth e1 e2 m = stackNeeded (m Map.! e1) (m Map.! e2)

computeStackMap :: Exp -> Map.Map Exp Integer
computeStackMap e = case e of
  ExpAdd e1 e2 -> Map.insert e (computeParentDepth e1 e2 m) m where
    m = Map.union (computeStackMap e1) (computeStackMap e2)
  ExpSub e1 e2 -> Map.insert e (computeParentDepth e1 e2 m) m where
    m = Map.union (computeStackMap e1) (computeStackMap e2)
  ExpMul e1 e2 -> Map.insert e (computeParentDepth e1 e2 m) m where
    m = Map.union (computeStackMap e1) (computeStackMap e2)
  ExpDiv e1 e2 -> Map.insert e (computeParentDepth e1 e2 m) m where
    m = Map.union (computeStackMap e1) (computeStackMap e2)
  ExpLit _ -> Map.singleton e 1
  ExpVar _ -> Map.singleton e 1


computeNeededStackStmt :: Stmt -> Integer
computeNeededStackStmt (SAss _ e) = (computeStackMap e) Map.! e
computeNeededStackStmt (SExp e) = (computeStackMap e) Map.! e

computeStackLimit :: Program -> Integer
computeStackLimit (Prog stmts) = foldr max 0 (map computeNeededStackStmt stmts)

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


computeBinaryOpIR :: Exp -> Exp -> Map.Map Exp Integer -> T.Text -> SState T.Text
computeBinaryOpIR e1 e2 m opBytecode = do
  t1 <- computeExpIR e1 m
  t2 <- computeExpIR e2 m
  if (m Map.! e1) >= (m Map.! e2)
    then return $ buildText [t1, t2, opBytecode]
    else return $ buildText [t2, t1, T.pack "swap", opBytecode]

-- second argument is stackMap, which maps exp -> stack depth to be used
computeExpIR :: Exp -> Map.Map Exp Integer -> SState T.Text
computeExpIR e m = do
  case e of
    ExpAdd e1 e2 -> computeBinaryOpIR e1 e2 m (T.pack "iadd")
    ExpSub e1 e2 -> computeBinaryOpIR e1 e2 m (T.pack "isub")
    ExpMul e1 e2 -> computeBinaryOpIR e1 e2 m (T.pack "imul")
    ExpDiv e1 e2 -> computeBinaryOpIR e1 e2 m (T.pack "idiv")
    ExpLit n -> return $ T.pack $ (push n) where
      push n
        | n == -1 = "iconst_m1"
        | n <= 5 && n >= 0 = "iconst_" ++ (show n)
        | n <= 2^7-1 && n >= -2^7 = "bipush " ++ (show n)
        | n <= 2^15-1 && n >= -2^15 = "sipush " ++ (show n)
        | otherwise = "ldc " ++ (show n)
    ExpVar (Ident x) -> do
      m <- gets snd
      case Map.lookup x m of
        Nothing -> error "Usage of not defined variable"
        Just num -> return $ T.pack $ iload ++ (show num) where
          iload = if num <= 3 then "iload_" else "iload "

computeStmtIR :: Stmt -> SState T.Text
computeStmtIR stmt = do
  case stmt of
    SAss (Ident x) e -> do
      modify (modifyAssignVariable x)
      let m = computeStackMap e
      t1 <- computeExpIR e m
      val <- get
      let xnum = getVariableNum x val
      let istore = if xnum <= 3 then "istore_" else "istore "
      return $ buildText [t1, T.pack $ istore ++ (show xnum)]

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

-- filename without extension as arg, to generate filename.class
buildIR :: Program -> FilePath -> T.Text
buildIR prog fname = buildText [buildProlog fname,
                                  buildConstructor,
                                  buildMain prog]



runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v f

run :: Verbosity -> FilePath -> String -> IO ()
run v fp s = do
  let ts = pProgram $ myLLexer s
  case ts of
           Bad s    -> putStrLn "\nParse failed...\n"
           Ok  tree -> do
             let ir = T.unpack $ buildIR tree (takeBaseName fp)
             let outJ = dropExtension fp <.> "j"
             writeFile outJ ir
             readProcess "java" ["-jar", "lib/jasmin.jar", "-d",
                                 takeDirectory outJ, outJ] ""
             return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [progPath] -> do
      runFile 0 progPath
    _ -> error "args error!"
