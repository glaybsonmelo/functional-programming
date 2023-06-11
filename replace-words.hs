-- MÓDULOS

import System.IO
import System.Environment

-- Esse programa ler
-- na linha de comando três strings.
-- A primeira é um nome de arquivo válido, 
-- digamos, f. As outras são palavras,
-- w1 e w2. O programa substitui
-- todas as aparições de w1 em f
-- por w2. Um arquivo de saída com as
-- modificações será criado.
-- o arquivo de saída tem nome igual ao de entrada, mas
-- com prefixo "subst-".

-- CÓDIGO

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fileName, w1, w2] -> do
            contents <- readFile fileName
            let modifiedContents = replaceAll w1 w2 contents
                outputFileName = "subst-" ++ fileName
            writeFile outputFileName modifiedContents
        _ -> putStrLn "Uso: atividade08 <arquivo> <w1> <w2>"

replaceAll :: String -> String -> String -> String
replaceAll _ _ [] = []
replaceAll from to str@(c:cs)
    | isPrefix from str = to ++ replaceAll from to (drop (length from) str)
    | otherwise = c : replaceAll from to cs

isPrefix :: Eq a => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys)
    | x == y = isPrefix xs ys
    | otherwise = False
