import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.List
import System.Console.Readline
import Data.Maybe

-- Lambda calculus AST definition
type Symbol = String

data Expr =
    Var Symbol |
    App Expr Expr |
    Fn Symbol Expr
       deriving Eq

-- Printing AST
instance Show Expr where
    show (Var v) = v
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Fn s e) = "(\\" ++ s ++ "." ++ show e ++ ")"


-- Parsing AST
exprParser = do exprs <- sepBy1 termParser (many space)
                return $ foldl1 (\e1 e2->App e1 e2) exprs

termParser = (between (char '(') (char ')') exprParser
              <?> "expression")
             <|> fnParser
             <|> symbolParser

symbolParser = do s <- many1 letter
                  return $ Var s
               <?> "symbol"

fnParser = do char '\\'
              (Var s) <- symbolParser
              char '.'
              e <- exprParser
              return $ Fn s e
           <?> "function"

parseLambda text = parse (do skipMany space
                             e <- exprParser
                             skipMany space >> eof
                             return e)
                         "" text

-- Reduction utilities
calcFreeVars e = calcFreeVars' e []
    where calcFreeVars' (Var s) b = case (elemIndex s b) of
                                      Just s -> []
                                      Nothing -> [s]
          calcFreeVars' (App e1 e2) b = calcFreeVars' e1 b ++ calcFreeVars' e2 b
          calcFreeVars' (Fn s e) b = calcFreeVars' e (s:b)

substitute (Var s) oldS newE
    | s == oldS = newE
substitute v@(Var s) oldS newE = v
substitute (App e1 e2) oldS newE = App (substitute e1 oldS newE)
                                       (substitute e2 oldS newE)
substitute fn@(Fn s e) oldS newE
    | s == oldS = fn
substitute (Fn s e) oldS newE = Fn s (substitute e oldS newE)

-- Alpha reduction
alphaReduce (Fn s e) oldS newS
    | s == oldS = Fn newS (substitute e oldS (Var newS))
alphaReduce (Fn s e) oldS newS = Fn s (alphaReduce e oldS newS)
alphaReduce e oldS newS = e

-- Beta reduction
betaReduce (App (Fn s e1) e2) = substitute (foldr (\s e->alphaReduce e s (gensym e)) e1 freeVars) s e2
    where freeVars = calcFreeVars e2
          boundVars e = let boundVars' (Fn s e) vs = boundVars' e (s:vs)
                            boundVars' (App e1 e2) vs = (boundVars' e1 vs) ++ (boundVars' e2 vs)
                            boundVars' e vs = vs
                        in boundVars' e []
          gensym e = fromJust $ find (not . (`elem` (boundVars e))) syms
          syms = [x:"" | x <- ['a' .. 'z']] ++ ['s':(show y) | y<-[1..]]
betaReduce (App e1 e2) = (App (betaReduce e1) (betaReduce e2))
betaReduce (Fn s e) = (Fn s (betaReduce e))
betaReduce e = e

-- Reduce to normal order
toNormalOrder e = unfoldr normalize e
    where normalize e = let reduced = betaReduce e
                        in if e == reduced
                           then Nothing
                           else Just (reduced, reduced)

-- Interaction loop
main = do s <- readline "> "
          case s of
            Nothing -> return ()
            Just s -> do let e = parseLambda s
                         case e of
                           Right e -> do putStrLn $ show e
                                         mapM_ (putStrLn . show) $ toNormalOrder e

                           Left e -> do putStrLn $ show e
                         main

