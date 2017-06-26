module Parser (parseSource) where

import Text.Parsec
import Type

pVar = do
    n <- many1 digit
    return (Var (read n :: Integer))

pApp = do
    char '('
    t1 <- pTerm
    t2 <- pTerm
    char ')'
    return (App t1 t2)

pAbs = do
    char '('
    spaces
    oneOf ['\\', 'l', 'λ']
    t <- pTerm
    char ')'
    return (Abs t)

pName = do
    name <- many1 upper
    return (Name name)

pChurch = do
    string "c"
    n <- many1 digit
    return (Church (read n :: Integer))

pTerm = do
    spaces
    t <- (pVar <|> try pApp <|> try pAbs <|> try pName <|> pChurch)
    spaces
    return t

pDef = do
    string "def"
    space
    spaces
    name <- many1 upper
    spaces
    char '='
    t <- pTerm
    char ';'
    return (Def name t)

pRun = do
    string "run"
    space
    spaces
    t <- pTerm
    char ';'
    return (Run t)

pShow = do
    string "show"
    space
    spaces
    t <- pTerm
    char ';'
    return (Show t)

pPrint = do
    string "print"
    space
    spaces
    s <- many $ noneOf ";\n"
    -- spaces
    char ';'
    return (Print s)

pComment = do
    char '#'
    many $ noneOf "\n"
    -- endOfLine
    newline
    return Comment
    
pLine = do
    spaces
    l <- (pDef <|> pRun <|> pShow <|> pPrint <|> pComment)
    -- l <- (pDef <|> pRun <|> pPrint <|> pComment)
    spaces
    return l

pScript = do
    ts <- many pLine
    eof
    return ts


parseSource s = parse pScript "" (s :: String)
