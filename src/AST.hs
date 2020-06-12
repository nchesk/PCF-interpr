module AST where
import Data.Function
import Data.List
import Control.Monad

data Term =
    TVar String |             --  Progress
    TNum Integer |            --  DONE
    TFun String Term |        --  DONE
    TApp Term Term |          --  TODO
    TPlus Term Term |         --  DONE
    TSub Term Term |          --  DONE
    TMult Term Term |         --  DONE
    TDiv Term Term |          --  DONE
    TIf Term Term Term |      --  DONE
    TFix String Term |        --  TODO
    TLet String Term Term     --  DONE
  deriving (Eq, Show)
-- Probably should use STATE monad to control
-- if so probably need a state machine

type Environment = 
    [ (String , Value)] -- should we have type Env when we extend env

data Value =
    ValInt Integer | 
    Closure String Term Environment |
    Thunk Term Environment
  deriving (Eq, Show)

extendEnv:: Environment -> String -> Value -> Environment
extendEnv env var val = (var, val):env

evalByValue :: Environment -> Term -> Value
evalByValue env (TPlus x y) =
    case ((evalByValue env x),(evalByValue env y)) of
        ((ValInt x), (ValInt y)) -> ValInt (x + y)
        _ -> error "Arguments of Plus function aren't numbers"
evalByValue env (TSub x y) =
    case ((evalByValue env x), (evalByValue env y)) of
        ((ValInt x),(ValInt y)) -> ValInt (x - y)
        _ -> error "Arguments of Substract function aren't numbers"
evalByValue env (TMult x y) =
    case ((evalByValue env x), (evalByValue env y)) of
        ((ValInt x),(ValInt y)) -> ValInt (x * y)
        _ -> error "Arguments of Multiply function aren't numbers"
evalByValue env (TDiv x y) =
    case ((evalByValue env x), (evalByValue env y)) of
        ((ValInt x), (ValInt y)) -> if (ValInt y) /= (ValInt 0) then ValInt(x `div` y) else error "Seems like you try to divide by zero"
        _ -> error "Arguments of Divide function aren't numbers"
evalByValue env (TIf t u v) = 
    case (evalByValue env t) of
        (ValInt t) -> if (ValInt t) == (ValInt 0) then (evalByValue env u)  else (evalByValue env v)
        _ -> error "Seems like something wrong with condition of if-statement"
evalByValue env (TNum n)=  ValInt n 
evalByValue env (TLet x t u) = evalByValue (extendEnv env x (evalByValue env t)) u 
evalByValue env (TFix x t) = evalByValue (extendEnv env x (Thunk (TFix x t) env)) t 
evalByValue env (TApp t u) = 
    case (evalByValue env t) of
        Closure x t1 e1 -> evalByValue (extendEnv e1 x (evalByValue env u)) t1
evalByValue env (TFun x t) = Closure x t env 
evalByValue env (TVar x) = case (lookup x env) of
    Just (Thunk a b) -> (evalByValue b a)
    Just (Closure s t e) -> (Closure s t e)
    Just (ValInt с) -> (ValInt с)
    Nothing -> error "TVar not in env"

evalByName :: Environment -> Term -> Value
evalByName env (TVar x) = case (lookup x env) of
    Just (Thunk t e1) -> (evalByName e1 t)
evalByName env (TApp t u) = case (evalByName env t) of
    Closure x t1 e1 -> evalByName (extendEnv e1 x (Thunk u env)) t1
evalByName env (TFun x t) = Closure x t env
evalByName env (TNum n) = ValInt n
evalByName env (TPlus t u) = case ((evalByValue env t), (evalByValue env u)) of
        ((ValInt t),(ValInt u)) -> ValInt (t + u)
        _ -> error "Arguments of Plus function aren't numbers"
evalByName env (TSub t u) = case ((evalByValue env t), (evalByValue env u)) of
        ((ValInt t),(ValInt u)) -> ValInt (t - u)
        _ -> error "Arguments of Substract function aren't numbers"
evalByName env (TMult t u) = case ((evalByValue env t), (evalByValue env u)) of
        ((ValInt t),(ValInt u)) -> ValInt (t * u)
        _ -> error "Arguments of Multiply function aren't numbers"
evalByName env (TDiv t u) = case ((evalByValue env t), (evalByValue env u)) of
        ((ValInt t), (ValInt u)) -> if (ValInt u) /= (ValInt 0) then ValInt(t `div` u) else error "Seems like you try to divide by zero"
        _ -> error "Arguments of Divide function aren't numbers"
evalByName env (TIf t u v) = case (evalByName env t) of
    ValInt n -> if n == 0 then (evalByName env u) else (evalByName env v)
evalByName env (TFix x t) = evalByName (extendEnv env x (Thunk (TFix x t) env)) t
evalByName env (TLet x t u) = evalByName (extendEnv env x (Thunk t env)) u



eval :: Term -> Value
eval t = evalByName [] t

fact = TFix (['f']) (TFun (['n']) (TIf (TVar (['n'])) (TNum 1) (TMult (TVar (['n'])) (TApp (TVar (['f'])) (TSub (TVar (['n'])) (TNum 1) ) ) )))
fact5 = TApp (fact) (TNum 5)

--Нумералы Черча
nthApp::Int -> String -> Term -> Term
nthApp n s z = if n == 0 then z else TApp (TVar s) (nthApp (n-1) s z)
intToChurch :: Int -> Term 
intToChurch n = TFun ("s") (TFun ("z") (nthApp n "s" (TVar ("z"))))
zeroChurch = TFun ("s") (TFun ("z") (TVar ("z")))
add1Term = TFun ("x") (TPlus (TVar ("x")) (TNum 1))

churchToInt :: Term -> Integer
churchToInt n = case (eval (TApp (TApp (n) (add1Term)) (TNum 0))) of
    ValInt i -> i
    _ -> error "WrongValue"

plusTerm = TFun ("m") (TFun ("n") (TFun ("s") (TFun ("z") (TApp (TApp (TVar ("m")) (TVar ("s"))) (TApp (TApp (TVar ("n")) ( TVar ("s"))) (TVar ("z")) )))))

timesTerm = TFun ("m") (TFun ("n") (TApp (TApp (TVar ("m")) (TApp (plusTerm) (TVar ("n")))) (zeroChurch)))

checkNumOp op m n = TApp (TApp (op) (intToChurch m)) (intToChurch n) & churchToInt
