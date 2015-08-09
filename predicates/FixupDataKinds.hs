import System.Environment
import Text.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.DeepSeq
import Data.Array


main = do
    [inFile, outFile] <- getArgs
    file <- readFile inFile
    let newContents = force $ unlines $ map processLine $ lines file
    writeFile outFile newContents


processLine :: String -> String
processLine input = let Right result = parse (fmap fixLine (try lineToFix) <|> regularLine) "input" input in result

regularLine = many anyChar

lineToFix :: Parsec String () (String, String, String)
lineToFix = do
    prefix <- anyChar `manyTill` (try $ string "\\text{\\tt")
    content <- anyChar `manyTill` (try $ string "--'}")
    suffix <- many anyChar
    return (prefix, content, suffix)

fixLine :: (String, String, String) -> String
fixLine (x, y, z) = x ++ fixContent y ++ z


fixContent content = let Right result = parse (many fixSymbol) "content" content in concat result

fixSymbol = try ("\\mathbin{\\text{'}\\!\\!:}" <$ string "':")
            <|> try ("\\text{'}[\\mskip1.5mu \\mskip1.5mu]" <$ string "'[]")
            <|> "\\mathrel{=}" <$ char '='
            <|> "\\;" <$ char '~'
            <|> do first <- upper; rest <- many alphaNum; return ("\\Conid{" ++ (first : rest) ++ "}")
            <|> do first <- lower; rest <- many alphaNum; return ("\\Varid{" ++ (first : rest) ++ "}")
            <|> fmap (: []) anyChar





-- --' is the magic string

-- Remove the whole \text{\tt} block and replace the contents:

-- ':   <==>   \mathbin{\text{'}\!\!:}
-- '[]  <==>   \text{'}[\mskip1.5mu \mskip1.5mu]
-- as   <==>   \Varid{as}
-- Here <==>   \Conid{Here}
-- =    <==>   \mathrel{=}
-- )    <==>   )
