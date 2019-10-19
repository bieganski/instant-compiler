import AbsInstant

import qualified Data.Text as T

default_superclass = "java/lang/Object"
main_signature = ".method public static main([Ljava/lang/String;)V"
default_constructor = [".method public <init>()V",
                       "  aload_0",
                       "  invokenonvirtual java/lang/Object/<init>()V",
                       "  return",
                       ".end method"]

main_begin = [".method public static main([Ljava/lang/String;)V"]
            
main_end = ["invokevirtual java/io/PrintStream/println(I)V",
            "return",
            ".end method"]

get_print_stream = "getstatic java/lang/System/out Ljava/io/PrintStream;"

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

buildMainContent :: Program -> T.Text
buildMainContent (Prog stmts) = T.pack "bipush 10"


buildMain :: Program -> T.Text
buildMain prog = buildText [buildMainBegin,
                                   limitStack 1000,
                                   limitLocals 1000,
                                   T.pack get_print_stream,
                                   buildMainContent prog,
                                   buildMainEnd]


emptyProgram :: Program
emptyProgram = Prog []



buildIR :: Program -> T.Text
buildIR prog = buildText [buildProlog "PrzykladowaKlasa",
                                  buildConstructor,
                                  buildMain prog]


main :: IO ()
main = do
  putStrLn $ T.unpack $ buildIR emptyProgram
