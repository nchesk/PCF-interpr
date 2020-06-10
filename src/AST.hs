module AST where

data Term =
	TVar String |							--	Progress
	TNum Integer |							--	DONE
	TFun String Term |						--	DONE
	TApp Term Term |						--	TODO
	TPlus Term Term |						--	DONE
	TSub Term Term |						--  DONE
	TMult Term Term |						--	DONE
	TDiv Term Term |						--	DONE
	TIf Term Term Term |					--	DONE
	TFix String Term |						--	TODO
	TLet String Term Term					--	DONE
  deriving (Eq, Show)	



type Environment = 
	[ (String , Integer)]

data Value =
	 ValInt Integer | 
	 Closure String Term Environment |
	 Thunk Term Environment
  deriving (Eq, Show)

searchInEnv :: Environment -> Term -> Value



evalByValue :: Environment -> Term -> Value
evalByValue env t 
	| t == TPlus x y =
		case (evalByValue env x) (evalByValue env y) of
				(ValInt x) (ValInt y) -> ValInt (x + y)
				_ -> error "Arguments of Plus function aren't numbers"
	| t == TSub x y =
		case (evalByValue env x) (evalByValue env y) of
				(ValInt x) (ValInt y) -> ValInt (x - y)
				_ -> error "Arguments of Substract function aren't numbers"
	| t == TMult x y =
		case (evalByValue env x) (evalByValue env y) of
				(ValInt x) (ValInt y) -> ValInt (x * y)
				_ -> error "Arguments of Multiply function aren't numbers"
	| t == TDiv x y =
		case (evalByValue env x) (evalByValue env y) of
				(ValInt x) (ValInt y) -> if (evalByValue env y) != 0 then ValInt(x/y) else error "Seems like you try to divide by zero"
				_ -> error "Arguments of Divide function aren't numbers"
	| t == TIf x y z  = 
		case (evalByValue env x) of
				(ValInt x) -> if (ValInt x) == 0 then (evalByValue env z)  else (evalByValue env y)
				_ -> error "Seems like something wrong with condition of if-statement"
	| t == TNum x = if isInteger x then ValInt x else error "NAN"
	| t == TLet x y z = evalByValue (extendEnv x (evalByValue env y)) z) -- TODO write extend function
	| t == TFix x y = evalByValue (extendEnv env ( x )) -- how to extend?
	| t == TApp x y = -- how to extend?
	| t == TFun x y = Closure x y env -- 
	| t == TVar x = -- search term with name x in env
					-- if x associated with ValInt exists in env then ValInt
					-- if x associated with thunk <fix a b, e'>then return result of evaluation evalByValue e' fix a b  
					-- TODO write search function
	| otherwise --throw an exception



