-- Lex.x -*- mode: haskell -*-
{
module Lex where
import Numeric
}

%wrapper "monad"

$alpha = [a-zA-Z_]
$digit = [0-9]

tokens :-
  $white+				;
  $digit+				{ \(p,_,_,s) len -> return $ TInt (read $ take len s) }

  [$alpha] [$alpha $digit]*	{ \(p,_,_,s) len -> return $ TIdent $ take len s }

  "+"					{ \(p,_,_,s) len -> return $ TPlus }
  "-"					{ \(p,_,_,s) len -> return $ TMinus }
  "*"					{ \(p,_,_,s) len -> return $ TMul }
  "="					{ \(p,_,_,s) len -> return $ TModifSet }
  ";" 					{ \(p,_,_,s) len -> return $ TSemiColon }
  "("					{ \(p,_,_,s) len -> return $ TLeftParen }
  ")"					{ \(p,_,_,s) len -> return $ TRightParen }
{

alexEOF = return TEOF

alexScanTokens str = runAlex str $ do
  let loop = do tok <- alexMonadScan
		if tok == TEOF
			then return [tok]
			else do toks <- loop
				return (tok:toks)
  loop

data Token =
	TInt Int | TIdent String
	| TPlus | TMinus | TMul | TModifSet | TSemiColon | TLeftParen | TRightParen
	| TEOF
	deriving (Eq)

instance Show Token where
  show x = case x of
    TInt i -> show i
    TIdent s -> s
    TModifSet -> "="
    TPlus -> "+"
    TMinus -> "-"
    TMul -> "*"
    TSemiColon -> ";"
    TLeftParen -> "("
    TRightParen -> ")"
    TEOF -> "(EOF)"
}
