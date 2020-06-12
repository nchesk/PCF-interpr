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
evalByValue env (TIf x y z) = 
    case (evalByValue env x) of
        (ValInt x) -> if (ValInt x) == (ValInt 0) then (evalByValue env z)  else (evalByValue env y)
        _ -> error "Seems like something wrong with condition of if-statement"
evalByValue env (TNum x)=  ValInt x 
evalByValue env (TLet x y z) = evalByValue (extendEnv env x (evalByValue env y)) z 
evalByValue env (TFix x y) = evalByValue (extendEnv env x (Thunk (TFix x y) env)) y 
evalByValue env (TApp x y) = 
    case (evalByValue env x) of
		Closure a b c -> evalByValue (extendEnv c a (evalByValue env y)) b
evalByValue env (TFun x y) = Closure x y env 
evalByValue env (TVar x) = case (lookup x env) of
    Just (Thunk a b) -> (evalByValue b a)
    Just (Closure s t e) -> (Closure s t e)
    Just (ValInt с) -> (ValInt с)
    Nothing -> error "Var not in env"

test =  TPlus (TNum 1) (TNum 2)

fact = TFix (['f']) (TFun (['f']) (TIf (TVar (['n'])) (TNum 1) (TMult (TVar (['n'])) (TApp (TVar (['f'])) (TSub (TVar (['n'])) (TNum 1) ) ) )))
fact5 = TApp ( fact) (TNum 5)


