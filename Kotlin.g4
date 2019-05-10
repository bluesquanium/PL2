/* Kotlin.g4 extended  */
grammar Kotlin;

// parser rules
prog : (packagePar | importPar | funPar | classPar | interfacePar | commentPar | valPar | varPar)* ;

// Defining packages
packagePar : PACKAGE ID ( '.' ID )* ;
importPar : IMPORT ID ( '.' ID )* ( '.*' )? ;

// Common Defining
prefixDef : OVERRIDE? ABSTRACT? ;
parameters : parameter ( ',' parameter )* ;
parameter : (VAL | VAR)? ID COLON type ; 
commentPar : COMMENT1 | COMMENT2 ;
type2Par : TYPE2 LANGLE TYPE1 RANGLE ;
type : (TYPE1 | type2Par) ;

// Defining functions
funPar : (prefixDef FUN | FUN) ID '(' parameters? ')' (COLON type ('?')? )?
	( '{' (expr | assn | con | con_expr | loop | whenPar | returnPar )* returnPar? '}'
	| '=' (expr | con_expr | whenPar | con) )?
	;
returnPar : RETURN expr? ; // "expr?" for nullable

// Defining variables
valPar : (prefixDef VAL | VAL) ID (COLON type)? ( 'get()'? '=' (expr | con) )? ;
varPar : (prefixDef VAR | VAR) ID (COLON type)? ( 'get()'? '=' (expr | con) )? ;

expr : expr OP expr
	| fun
	| NULL
	| num
	| ID '.' ID
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
con : con (OP3 | '>' | '<') con
	| expr
	| '(' con ')'
	| ID ('!')? IS type
	;
fun : (ID'.')* ID '(' (expr ( ',' expr )*)? ')' ;
num : INT
	| REAL
	;

// Defining Conditional Expression
//range : expr '..' expr (STEP INT)?
range : expr '..' expr (STEP INT)?
	| ('0..' | '1..' | '2..' | '3..' | '4..' | '5..' | '6..' | '7..' | '8..' | '9..' | '10..') expr (STEP INT)?
	| expr 'downTo' expr (STEP INT)?
	| ID '.' ID
	| ID
	;
con_expr : IF '(' con ')' ( '{' (expr | assn | con_expr | loop | whenPar)* returnPar? '}' | (expr | assn | whenPar | returnPar) )
	( ELSE IF '(' con ')' ( '{' (expr | assn | con_expr | loop | whenPar)* returnPar? '}' | (expr | assn | whenPar | returnPar) ) )*
	( ELSE ( '{' (expr | assn | con_expr | loop | whenPar)* returnPar? '}' | (expr | assn | whenPar | returnPar) ) )?; // Ambiguous for "con_expr, loop" when the case of non "{}"

// Defining Loop
loop : FOR '(' ID ('!')? IN range ')' '{' (expr | assn | con_expr | loop | whenPar | returnPar)* '}'
	| WHILE '(' con ')' '{' (expr | assn | con_expr | loop | whenPar | returnPar)* '}'
	;

// Defining When
when_expr : expr (',' expr)* ARROW (expr | returnPar) 
	| ('!')? IN range ARROW (expr | returnPar) // Add '..' later!!
	;
whenPar : WHEN '(' expr ')' '{' when_expr+ (ELSE ARROW expr)? '}' ;

// Defining Class
//classParameters : classParameter ( ',' classParameter )* ;
//classParameter : (VAR | VAL) ID COLON type ;
classPar : (prefixDef CLASS | CLASS) ID '(' parameters ')'
    ( COLON ID ('(' (expr ( ',' expr )*)? ')')? (',' ID ('(' (expr ( ',' expr )*)? ')')? )* )?
    '{' (assn ','? | funPar ','?)* '}' ;

// Defining Interface
interfacePar : INTERFACE ID '{' (assn ','? | funPar ','?)* '}' ;

// lexer rules
IMPORT: 'import' ;
PACKAGE: 'package' ;
CLASS: 'class' ;
ABSTRACT: 'abstract' ;
INTERFACE: 'interface' ;
OVERRIDE: 'override' ;
FUN: 'fun' ;
VAR: 'var' ;
VAL: 'val' ;
IF: 'if' ;
ELSE: 'else' ;
FOR: 'for' ;
IN: 'in' ;
IS: 'is' ;
STEP: 'step' ;
WHILE: 'while' ;
WHEN: 'when' ;
ARROW: '->' ;
RETURN: 'return' ;
NULL: 'null' ;
TYPE1 : 'Int'
	| 'Short'
	| 'Byte'
	| 'Long'
	| 'Float'
	| 'Double'
	| 'Char'
	| 'Boolean'
	| 'String'
	| 'Unit' // almost same with 'void'
	| 'Nothing'
	;
TYPE2 : 'List'
	| 'MutableList'
	| 'ArrayList'
	| 'Map'
	| 'MutableMap'
	| 'HashMap'
	| 'LinkedHashMap'
	| 'SortedMap'
	| 'Set'
	| 'MutableSet'
	| 'HashSet'
	| 'LinkedHashSet'
	| 'TreeSet'
	| 'Array'
	;

LANGLE: '<' ;
RANGLE: '>' ;
LPAREN: '(' ;
RPAREN: ')' ;
LCURL: '{' ;
RCURL: '}' ;
LSQUARE: '[' ;
RSQUARE: ']' ;
COLON: ':' ;

OP_LANGLE : LANGLE -> type(LANGLE) ;
OP_RANGLE : RANGLE -> type(RANGLE) ;

COMMENT1 : '/' '/' ~[\r\n]* -> skip ;
COMMENT2 : '/' '*' .*? '*' '/' -> skip ;
OP : 	  '*'
	| '/'
	| '+'
	| '-'
	| '%'
	;
OP2 :	  '+='
	| '-='
	| '*='
	| '/='
	| '%='
	;
OP3 : '=='
	| '!='
	| OP_LANGLE
	| OP_RANGLE
	| '<='
	| '>='
	| '&&'
	| '||'
	;

//NEWLINE: [\r\n]+ ;
LP_RANGE: [0-9]+[.][.] ;
ID: [a-zA-Z_$][0-9a-zA-Z_$]* ;
INT: [-+]?[0-9]+ ;
REAL: [-+]?[0-9]+'.'[0-9]* ;
STRING: (['] ~[']* [']) | (["] ~["]* ["]) ;
WS: [ \t\r\n]+ -> skip ;
