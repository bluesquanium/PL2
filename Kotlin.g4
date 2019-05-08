/* Kotlin.g4 extended  */
grammar Kotlin;

// parser rules
prog : (packagePar | importPar | funPar | commentPar | valPar | varPar)* ;

// Defining packages
packagePar : PACKAGE ID ( '.' ID )* ;
importPar : IMPORT ID ( '.' ID )* ( '.*' )? ;

// Defining functions
funPar : FUN ID '(' parameters? ')' (COLON TYPE ('?')? )? '{' (expr | assn | con_expr | loop)* returnPar? '}'
	| FUN ID '(' parameters? ')' '=' (expr | con_expr)
	;
parameters : parameter ( ',' parameter )* ;
parameter : ID COLON TYPE ; 
returnPar : RETURN expr? ; // "expr?" for nullable
commentPar : COMMENT1 | COMMENT2 ;

// Defining variables
valPar : VAL ID (COLON TYPE)? '=' expr ;
varPar : VAR ID '=' expr ;

expr : expr OP expr
	| fun
	| num
	| ID
	| STRING
	| '(' expr ')'
	| expr ('++' | '--')
	| ('++' | '--' | '!') expr
	;
assn : valPar
	| varPar
	| ID '=' expr
	| ID OP2 expr
	;
con_expr : IF '(' expr OP3 expr ')' ( '{' (expr | assn | con_expr | loop)* returnPar? '}' | (expr | assn | returnPar) )
	( ELSE IF '(' expr OP3 expr ')' ( '{' (expr | assn | con_expr | loop)* returnPar? '}' | (expr | assn | returnPar) ) )*
	( ELSE ( '{' (expr | assn | con_expr | loop)* returnPar? '}' | (expr | assn | returnPar) ) )?; // Ambiguous for "con_expr, loop" when the case of non "{}"
loop : FOR '(' ID IN ID ')' '{' (expr | assn | con_expr | loop | returnPar)* '}'
	| WHILE '(' expr OP3 expr ')' '{' (expr | assn | con_expr | loop | returnPar)* '}'
	;
fun : ID '(' expr ( ',' expr )* ')' ;
num : INT
	| REAL
	;

// lexer rules
IMPORT: 'import' ;
PACKAGE: 'package' ;
CLASS: 'class' ;
FUN: 'fun' ;
VAR: 'var' ;
VAL: 'val' ;
IF: 'if' ;
ELSE: 'else' ;
FOR: 'for' ;
IN: 'in' ;
WHILE: 'while' ;
RETURN: 'return' ;
TYPE : 'Int'
	| 'Short'
	| 'Byte'
	| 'Long'
	| 'Float'
	| 'Double'
	| 'Char'
	| 'Boolean'
	| 'Unit' // almost same with 'void'
	| 'Nothing'
	;

COMMENT1 : '/' '/' ~[\r\n]* -> skip ;
COMMENT2 : '/' '*' .*? '*' '/' -> skip ;
OP : '*'
	| '/'
	| '+'
	| '-'
	;
OP2 : '+='
	| '-='
	| '*='
	| '/='
	;
OP3 : '=='
	| '!='
	| '<'
	| '>'
	| '<='
	| '>='
	;

LPAREN: '(' ;
RPAREN: ')' ;
LCURL: '{' ;
RCURL: '}' ;
LSQUARE: '[' ;
RSQUARE: ']' ;
COLON: ':' ;

//NEWLINE: [\r\n]+ ;
ID: [a-zA-Z_$][0-9a-zA-Z_$]* ;
INT: [-+]?[0-9]+ ;
REAL: [-+]?[0-9]+'.'[0-9]* ;
STRING: (['] ~[']* [']) | (["] ~["]* ["]) ;
WS: [ \t\r\n]+ -> skip ;
