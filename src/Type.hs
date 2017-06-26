module Type (Term(..), Line(..), Script(..)) where

data Term = Var Integer | Abs Term | App Term Term | Name String | Church Integer
instance Show Term where
    show (Var n)                    = show n
    show (Abs (Name name))           = "(λ " ++ name ++ ")"
    show (Abs t1)                   = "(λ" ++ show t1 ++ ")"
    show (App (Var t1) (Var t2))    = "("  ++ show t1 ++ " " ++ show t2 ++ ")"
    show (App (Name n1) (Name n2))  = "("  ++ n1 ++ " " ++ n2 ++ ")"
    show (App (Name n1) t2)         = "("  ++ n1 ++ " " ++ show t2  ++ ")"
    show (App t1 (Name n2))         = "("  ++ show t1 ++ " " ++ n2 ++ ")"
    show (App t1 t2)                = "("  ++ show t1 ++ show t2 ++ ")"
    show (Name name)                = name
    show (Church n)                 = "[c" ++ show n ++  "]"


data Line = Def String Term
            | Run Term
            | Show Term
            | Print String
            | Comment
                deriving (Show)

type Script = [Line]
