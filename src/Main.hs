import System.IO
import System.Environment
import Data.Map

import Parser
import Type

betaReduction :: Term -> Term
betaReduction (App (Abs t1) t2) = betaReduction $ substitution t1 t2
betaReduction (App t1 t2)       = case betaReduction t1 of t1'@(Abs _)    -> betaReduction $ App t1' t2
                                                           t1'            -> App t1' $ betaReduction t2
betaReduction (Abs t1)          = Abs (betaReduction t1)
betaReduction (Var n)           = Var n

substitution :: Term -> Term -> Term
substitution t1 t2 = rec t1 1
    where rec (Var n) depth     = if n == depth then addFreeVariable t2 $ depth - 1
                                  else if n > depth then Var (n - 1)
                                  else Var n
          rec (Abs t1) depth    = Abs (rec t1 $ depth + 1)
          rec (App t1 t2) depth = App (rec t1 depth) (rec t2 depth)

addFreeVariable :: Term -> Integer -> Term
addFreeVariable t d = rec t 0
    where rec (Var n) depth     = if n > depth then Var (n + d) else Var n
          rec (Abs t1) depth    = Abs (rec t1 $ depth + 1)  
          rec (App t1 t2) depth = App (rec t1 depth) (rec t2 depth)



lMULT   = Abs (Abs (Abs (App (Var 3) (App (Var 2) (Var 1)))))

main = do
        args    <- getArgs
        let filename = head args
        handle  <- openFile filename ReadMode
        contents    <- hGetContents handle 
        let results = parseSource contents
        case results of
            Left e      -> do 
                            putStrLn "Syntax Error"
                            return ()
            Right ops   -> do
                            eval (ops, empty)
                            return ()
        hClose handle
        return ()

type Table = Map String Term

replace :: Term -> Table -> Term
replace t table = case t of
                    Name name   -> table ! name
                    Church n    -> church n
                    App t1 t2   -> App (replace t1 table) (replace t2 table)
                    Abs t       -> Abs (replace t table)
                    t           -> t 

church :: Integer -> Term
church n
    | n < 0     = church 0
    | otherwise = Abs (Abs (rec n))
    where rec 0 = Var 1
          rec n = App (Var 2) (rec $ n - 1)

eval ([], _)           = return ()
eval ((op:ops), table) = do
                            case op of
                                Def name t  -> do
                                                let t' = replace t table
                                                eval (ops, insert name t' table)
                                Run t       -> do
                                                let t' = replace t table
                                                putStrLn $ show $ betaReduction t'
                                                eval (ops, table)
                                Show t      -> do 
                                                let t' = replace t table
                                                putStrLn $ show t'
                                                eval (ops, table)

