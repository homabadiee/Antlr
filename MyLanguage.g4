grammar MyLanguage;

start
    : importState* class+ EOF;

importState
    : IMPORT ':' ':' FROM '(' library_name (',' CLASS_NAME)+ ')' '.' ';' ';'
    | IMPORT ':' ':' FROM '(' library_name ',' '*' ')' '.' ';' ';' ;

library_name
    : NAME | CLASS_NAME | VAR_NAME | CHAR;

class
    : (ACESSLEVEL)? CLASS_NAME CLASS
      (INHERITED FROM CLASS_NAME (', ' CLASS_NAME)* )?
      (IMPLEMENTS interface_name (', ' interface_name)* )?
       BEGIN
       (class_body ';'?)*
       END
       ';'? ;

interface_name
    : NAME | CLASS_NAME | VAR_NAME | CHAR;

class_body
    : array | reference | variable | assignment | function | procedure ;

function
    : (ACESSLEVEL)? (STATIC)? FUNC function_name
    '(' ( (parameter_name data_type ';' | VAR parameter_name ':' data_type ';')+
     (parameter_name data_type | VAR parameter_name ':' data_type ) )* ')'
     RETURNS data_type
     '{'
      (body ';'?)*
      (RETURN '(' expression ')' ';')?
      '}' ';'? ;

function_name
    : PRINT | NAME | CLASS_NAME | VAR_NAME | CHAR;

function_call
    : function_name '('(parameter (', ' parameter)*)?')' ';'?;

parameter_name
    : VAR_NAME ;

procedure
    : (ACESSLEVEL)? (STATIC)? PROC procedure_name
    '(' (body ';'?)* ')' ';' ;

procedure_name
    : NAME | CLASS_NAME | VAR_NAME | CHAR;

body
    : for | while | do_as_long_as | if | question_colon | switch_case |
     try_exception | function_call |  expression | assignment | variable | array | reference ;

variable
    : (ACESSLEVEL)? (CONSTANT)? VAR (variable_name (', ' variable_name)*)
    ':' data_type ( '=' value )? ';'? ;

variable_name
    : VAR_NAME ;

array
    : (ACESSLEVEL)? (CONSTANT)? VAR
    ('['(ZERO ':' POS_DECIMAL)?']')? (array_name (', ' array_name)*) ':' return_type
    ('=' '{' '[' (value)? (', ' value)* ']' '}') ?  ';'?;

array_name
    : NAME | CLASS_NAME | VAR_NAME | CHAR;

return_type
    :  data_type | VOID ;

reference
    : (ACESSLEVEL)? CLASS_NAME ('['']')? reference_name '->'
    ((NEW CLASS_NAME ('['(ZERO ':' POS_DECIMAL)?']')? '(' parameter? (',' parameter)* ')') | NULL) ';'?;

reference_name
    : NAME | CLASS_NAME | VAR_NAME | CHAR ;

parameter
    : array | variable_name | value;

for
    : (FOR '(' (variable)? (';' condition)? (';' increment | decrement)?')'
       (('{' (body ';'?)+ '}' ';'?) | '{'? body? '}'? ';')
       |
       FOR EVERY (ACESSLEVEL)? VAR variable_name IN array_name
       DO (body ';'?)+ UNTIL END );

increment
    : PRE_INCREMENT | POST_INCREMENT ;

decrement
    : PRE_DECREMENT | POST_DECREMENT ;

while
    : WHILE '(' condition* ')'
    BEGIN (body ';'?)* END ';'? ;

do_as_long_as
    : DO (body ';'?)* AS'_'LONG'_'AS condition+ ';' ;

if
    : IF '{' condition+ '}' THEN (body ';'?)* ';'
    (ELIF '(' condition+ ')' DO (body ';'?)* END)?
    (ELSE DO AS FOLLOW (body ';'?)* STOP)? ;

question_colon
    : condition '?' (body ';'?)* ':' (body ';'?)* ';'? ;

switch_case
    : SWITCH expression MATCH
    (CASE case ':' (body ';'?)* (BREAK';')?)+
    (DEFAULT case ':' (body ';'?)*)? END ;

case
    : value ;

try_exception
    : TRY '{{' (body ';'?)* '}}'CATCH
    '('(error_class_name error_name)? (',' error_class_name error_name)*')'
    BEGIN (body ';'?)* END ;

error_class_name
    : CLASS_NAME;

error_name
    : reference_name ;

assignment
    : variable_name '<-' (value | expression) ';'? ;

expression
    :  '(' expression (NEGATIVE?)')'
    | expression '^' expression
    | unaryExpression
    | expression ('<<' | '>>') expression
    | expression ('*' | '/' | '//' | '%' ) expression
    | expression ('+' | '-') expression
    | expression '&&' expression
    | expression ('&'| AND ) expression
    | expression '||' expression
    | expression ('|'| OR ) expression
    | expression ('==' | '!=') expression
    | expression ('<=' | '>=' ) expression
    | expression ('<' | '>' ) expression
    | expression ('+=' | '-=') expression
    | expression ('*=' | '/=') expression
    | VAR_NAME | decimal | FLOAT;


unaryExpression
    : NEGATIVE |PRE_INCREMENT | POST_INCREMENT | PRE_DECREMENT | POST_DECREMENT  ;


condition
    :  boolean
    | expression '&&' expression
    | expression ('&'| AND ) expression
    | expression '||' expression
    | expression ('|'| OR ) expression
    | expression ('==' | '!=') expression
    | expression ('<=' | '>=' ) expression
    | expression ('<' | '>' ) expression
    | expression ('+=' | '-=') expression
    | expression ('*=' | '/=') expression;

data_type
    : Integer | Float | Char | Boolean | String | NULL ;

value
    : integer | FLOAT | char | boolean | string | NULL;

integer
    : decimal | HEX_DECIMAL | BINARY ;

string
    : STRING;

char
    : '"'CHAR'"' | '‘'CHAR'’';

boolean
    : TRUE | FALSE ;

decimal
    : ZERO | POS_DECIMAL | NEG_DECIMAL;


// ----------------------- LEXER RULES --------------------- //

WS : [ \n\t\r]+ -> skip ;
SINGLE_LINE_COMMENT : '--' .*? '\r'? '\n' -> skip ;
MULTI_LINE_COMMENT : '!#' .*? '!#' -> skip ;
fragment DIGIT : [0-9] ;
IF : ('I' | 'i')('F' | 'f') ;
AS : ('A' | 'a')('S' | 's') ;
DO : ('D' | 'd')('O' | 'o') ;
IN : ('I' | 'i')('N' | 'n') ;
OR : ('O' | 'o')('R' | 'r') ;
THEN : ('T' | 't')('H' | 'h')('E' | 'e')('N' | 'n') ;
ELIF : ('E' | 'e')('L' | 'l')('I' | 'i')('F' | 'f') ;
ELSE : ('E' | 'e')('L' | 'l')('S' | 's')('E' | 'e') ;
FOLLOW : ('F' | 'f')('O' | 'o')('L' | 'l')('L' | 'l')('O' | 'o')('W' | 'w') ;
STOP : ('S' | 's')('T' | 't')('O' | 'o')('P' | 'p') ;
LONG : ('L' | 'l')('O' | 'o')('N' | 'n')('G' | 'g') ;
IMPORT : ('I' | 'i')('M' | 'm')('P' | 'p')('O' | 'o')('R' | 'r')('T' | 't') ;
FROM : ('F' | 'f')('R' | 'r')('O' | 'o')('M' | 'm') ;
CLASS : ('C' | 'c')('L' | 'l')('A' | 'a')('S' | 's')('S' | 's') ;
INHERITED : ('I' | 'i')('N' | 'n')('H' | 'h')('E' | 'e')('R' | 'r')('I' | 'i')('T' | 't')('E' | 'e')('D' | 'd');
IMPLEMENTS : ('I' | 'i')('M' | 'm')('P' | 'p')('L' | 'l')('E' | 'e')('M' | 'm')('E' | 'e')('N' | 'n')('T' | 't')('S' | 's') ;
BEGIN : ('B' | 'b')('E' | 'e')('G' | 'g')('I' | 'i')('N' | 'n') ;
END : ('E' | 'e')('N' | 'n')('D' | 'd') ;
STATIC : ('S' | 's')('T' | 't')('A' | 'a')('T' | 't')('I' | 'i')('C' | 'c') ;
FUNC : ('F' | 'f')('U' | 'u')('N' | 'n')('C' | 'c') ;
VAR : ('V' | 'v')('A' | 'a')('R' | 'r') ;
RETURNS : ('R' | 'r')('E' | 'e')('T' | 't')('U' | 'u')('R' | 'r')('N' | 'n')('S' | 's') ;
RETURN : ('R' | 'r')('E' | 'e')('T' | 't')('U' | 'u')('R' | 'r')('N' | 'n') ;
PROC : ('P' | 'p')('R' | 'r')('O' | 'o')('C' | 'c') ;
CONSTANT : ('C' | 'c')('O' | 'o')('N' | 'n')('S' | 's')('T' | 't')('A' | 'a')('N' | 'n')('T' | 't') ;
NEW : ('N' | 'n')('E' | 'e')('W' | 'w') ;
NULL : ('N' | 'n')('U' | 'u')('L' | 'l')('L' | 'l') ;
FOR : ('F' | 'f')('O' | 'o')('R' | 'r') ;
EVERY : ('E' | 'e')('V' | 'v')('E' | 'e')('R' | 'r')('Y' | 'y') ;
UNTIL : ('U' | 'u')('N' | 'n')('T' | 't')('I' | 'i')('L' | 'l') ;
WHILE : ('W' | 'w')('H' | 'h')('I' | 'i')('L' | 'l')('E' | 'e') ;
SWITCH : ('S' | 's')('W' | 'w')('I' | 'i')('T' | 't')('C' | 'c')('H' | 'h') ;
MATCH : ('M' | 'm')('A' | 'a')('T' | 't')('C' | 'c')('H' | 'h') ;
CASE : ('C' | 'c')('A' | 'a')('S' | 's')('E' | 'e') ;
BREAK : ('B' | 'b')('R' | 'r')('E' | 'e')('A' | 'a')('K' | 'k') ;
DEFAULT : ('D' | 'd')('E' | 'e')('F' | 'f')('A' | 'a')('U' | 'u')('L' | 'l')('T' | 't') ;
TRY : ('T' | 't')('R' | 'r')('Y' | 'y') ;
CATCH : ('C' | 'c')('A' | 'a')('T' | 't')('C' | 'c')('H' | 'h') ;
TRUE : ('T' | 't')('R' | 'r')('U' | 'u')('E' | 'e') ;
FALSE : ('F' | 'f')('A' | 'a')('L' | 'l')('S' | 's')('E' | 'e') ;
AND : ('A' | 'a')('N' | 'n')('D' | 'd') ;
VOID : ('V' | 'v')('O' | 'o')('I' | 'i')('D' | 'd') ;
PRINT : ('P' | 'p')('R' | 'r')('I' | 'i')('N' | 'n')('T' | 't');
// data Type
Integer : ('I' | 'i')('N' | 'n')('T' | 't')('E' | 'e')('G' | 'g')('E' | 'e')('R' | 'r') ;
Float : ('F' | 'f')('L' | 'l')('O' | 'o')('A' | 'a')('T' | 't') ;
Char : ('C' | 'c')('H' | 'h')('A' | 'a')('R' | 'r') ;
Boolean : ('B' | 'b')('O' | 'o')('O' | 'o')('L' | 'l')('E' | 'e')('A' | 'a')('N' | 'n') ;
String : ('S' | 's')('T' | 't')('R' | 'r')('I' | 'i')('N' | 'n')('G' | 'g') ;


PRE_INCREMENT : '++'VAR_NAME;
POST_INCREMENT : VAR_NAME'++';
PRE_DECREMENT : '--'VAR_NAME;
POST_DECREMENT : VAR_NAME'--';
NEGATIVE : '-'VAR_NAME;


ACESSLEVEL : DIRECTACCESS | INDIRECTACCESS | RESTRICTED ;
DIRECTACCESS : ('D' | 'd')('I' | 'i')('R' | 'r')('E' | 'e')('C' | 'c')('T' | 't')('A' | 'a')('C' | 'c')('C' | 'c')('E' | 'e')('S' | 's')('S' | 's') ;
INDIRECTACCESS : ('I' | 'i')('N' | 'n')('D' | 'd')('I' | 'i')('R' | 'r')('E' | 'e')('C' | 'c')('T' | 't')('A' | 'a')('C' | 'c')('C' | 'c')('E' | 'e')('S' | 's')('S' | 's') ;
RESTRICTED : ('R' | 'r')('E' | 'e')('S' | 's')('T' | 't')('R' | 'r')('I' | 'i')('C' | 'c')('T' | 't')('E' | 'e')('D' | 'd') ;
ZERO : '0';

POS_DECIMAL : [1-9]+DIGIT* ;
NEG_DECIMAL : '-'POS_DECIMAL ;
HEX_DECIMAL : '0'('X' | 'x')('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |'A' | 'a' | 'B' | 'b' | 'C' | 'c' | 'D' | 'd' | 'E' | 'e' | 'F' | 'f')+;
BINARY : ('0' | '1')+('B' | 'b');
FLOAT : DIGIT+'.'DIGIT+(('e')('-')?)?DIGIT*;

STRING : '"'.*?'"' |'‘'.*?'’' ;
VAR_NAME : ([a-z])([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?([a-zA-Z0-9_])?;
CHAR: [a-zA-Z_$];
CLASS_NAME : [A-Z]+([a-zA-Z0-9_$])*;
NAME : [a-zA-Z0-9_$]+;
