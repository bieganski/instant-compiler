import AbsInstant

import Data.Text

default_superclass = "java/lang/object"

-- class name -> prolog string
buildWrappingClass :: String -> String
buildWrappingClass cname = ".class " ++ cname

main :: IO ()
main = do
  putStrLn $ buildWrappingClass "Lol"
