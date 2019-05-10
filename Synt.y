-- Synt.y -*- mode: haskell -*-
{
module Synt where
import Lex
}

%name synt
%tokentype { Token }
%monad { Either String } { (>>=) } { return }

%token
	int				{ TInt $$ }
	ident 			{ TIdent $$ }
	'+'				{ TPlus }
	'-'				{ TMinus }
	'*'				{ TMul }
	'='				{ TModifSet }
	';' 			{ TSemiColon }
	'('				{ TLeftParen }
	')' 			{ TRightParen }
	'EOF'			{ TEOF }

%right '='
%left '+' '-'
%left '*'
%right NEG POS
%%

Program:
	ExprList			{ Program $1 }

ExprList:
	Expr ';' ExprList		{ ExprList $1 $3 }
	| 'EOF'				{ ExprEnd }

Expr:
	ident '=' RVal			{ Expr $1 $3 }

RVal:
	RVal '+' RVal			{ BinOp Add $1 $3 }
	| RVal '-' RVal			{ BinOp Sub $1 $3 }
	| RVal '*' RVal			{ BinOp Mul $1 $3 }
	| '-' RVal %prec NEG		{ UnOp Neg $2 }
	| '+' RVal %prec POS		{ UnOp Pos $2 }
	| '(' RVal ')'			{ $2 }
	| int				{ IntVal $1 }
	| ident				{ IdentVal $1 }

{

happyError _ = Left "  syntax error\n"

data Program = Program ExprList
		deriving (Show, Eq)

data ExprList =	ExprList Expr ExprList | ExprEnd
		deriving (Show, Eq)

data Expr =	Expr String RVal
		deriving (Show, Eq)

data BinOpType = Add | Sub | Mul
		deriving (Show, Eq)

data UnOpType = Neg | Pos
		deriving (Show, Eq)

data RVal = IntVal Int | IdentVal String
		| BinOp BinOpType RVal RVal | UnOp UnOpType RVal
		deriving (Show, Eq)
}
