%{
#include <stdio.h>
#include <stdlib.h>

extern int yylex();
extern char* yytext;
extern int yyparse();
extern int yylineno;
void yyerror(const char*);
extern FILE* yyin;
%}

%union 
{
    char* str;
}

%token FUNCTION INTEGER ARRAY OF READ WRITE RETURN
%token BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY
%token BEGINLOOP ENDLOOP
%token IF THEN ENDIF ELSE WHILE DO CONTINUE
%token AND OR NOT TRUE FALSE
%token SUB ADD MULT DIV MOD
%token EQ NEQ LT GT LTE GTE
%token IDENT NUMBER
%token SEMICOLON COLON COMMA ASSIGN
%token L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET

%left FUNCTION ARRAY
%left SUB ADD MULT DIV MOD
%left EQ NEQ LT GT LTE GTE
%left AND OR
%right NOT ASSIGN

%precedence NEG

%type<str> IDENT

%start program

%%
program:
    functions                           { printf("program -> functions\n"); }
;

functions:
    function functions                  { printf("functions -> function functions\n"); }
|   %empty                              { printf("functions -> epsilon\n"); }
;

function:
    FUNCTION identifiers SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY  { printf("function -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n"); }
;

declarations:
    declaration SEMICOLON declarations  { printf("declarations -> declaration SEMICOLON declarations\n"); }
|   %empty                              { printf("declarations -> epsilon\n"); }
;

declaration:
    identifiers COLON INTEGER           { printf("declaration -> identifiers COLON INTEGER\n"); }
|   identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER                                 { printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n"); }
;

identifiers:
    ident                               { printf("identifiers -> ident\n"); }
|   ident COMMA identifiers             { printf("identifiers -> ident COMMA identifiers\n"); }
;

ident:
    IDENT                               { printf("ident -> IDENT %s\n", $1); }
;

statements:
    statement SEMICOLON statements      { printf("statements -> statement SEMICOLON statements\n"); }
|   %empty                              { printf("statements -> epsilon\n"); }
;

statement:
    var ASSIGN expression               { printf("statement -> var ASSIGN expression\n"); }
|   IF bool-expr THEN statements ENDIF  { printf("statement -> IF bool-expr THEN statements ENDIF\n"); }
|   IF bool-expr THEN statements ELSE statements ENDIF { printf("statement -> IF bool-expr THEN statements ELSE statements ENDIF\n"); }
|   WHILE bool-expr BEGINLOOP statements ENDLOOP  { printf("statement -> WHILE bool-expr BEGINLOOP statements ENDLOOP\n"); }
|   DO BEGINLOOP statements ENDLOOP WHILE bool-expr  { printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE bool-expr\n"); }
|   READ vars                           { printf("statement -> READ vars\n"); }
|   WRITE vars                          { printf("statement -> WRITE vars\n"); }
|   CONTINUE                            { printf("statment -> CONTINUE\n"); }
|   RETURN expression                   { printf("statement -> RETURN expression\n"); }
;

bool-expr:
    relation-and-expr                   { printf("bool-expr -> relation-and-expr\n"); }
|   relation-and-expr OR bool-expr      { printf("bool-expr -> relation-and-expr OR relation-and-expr\n"); }
;

relation-and-expr:
    relation-expr                       { printf("relation-and-expr -> relation-expr\n"); }
|   relation-expr AND relation-and-expr { printf("relation-and-expr -> relation-expr AND relation-expr\n"); }
;

relation-expr:
    expression comp expression          { printf("relation-expr -> expression comp expression\n"); }
|   NOT expression comp expression      { printf("relation-expr -> NOT expression comp expression\n"); }
|   TRUE                                { printf("relation-expr -> TRUE\n"); }
|   NOT TRUE                            { printf("relation-expr -> NOT TRUE\n"); }
|   FALSE                               { printf("relation-expr -> FALSE\n"); }
|   NOT FALSE                           { printf("relation-expr -> NOT FALSE\n"); }
|   L_PAREN bool-expr R_PAREN           { printf("relation-expr -> L_PAREN bool-expr R_PAREN\n"); }
|   NOT L_PAREN bool-expr R_PAREN       { printf("relation-expr -> NOT L_PAREN bool-expr R_PAREN\n"); }
;

comp:
    EQ                                  { printf("comp -> EQ\n"); }
|   NEQ                                 { printf("comp -> NEQ\n"); }
|   LT                                  { printf("comp -> LT\n"); }
|   GT                                  { printf("comp -> GT\n"); }
|   LTE                                 { printf("comp -> LTE\n"); }
|   GTE                                 { printf("comp -> GTE\n"); }
;

expressions:
    expression                          { printf("expressions -> expression\n"); }
|   expression COMMA expressions        { printf("expressions -> expression COMMA expressions\n"); }
|   %empty                              { printf("expressions -> epsilon\n"); }
;

expression:
    multiplicative-expr                 { printf("expression -> multiplicative-expr\n"); }
|   multiplicative-expr ADD expression  { printf("expression -> multiplicative-expr ADD multiplicative-expr\n"); }
|   multiplicative-expr SUB expression  { printf("expression -> multiplicative-expr SUB multiplicative-expr\n"); }
;

multiplicative-expr:
    term                                { printf("multiplicative-expr -> term\n"); }
|   term MULT multiplicative-expr       { printf("multiplicative-expr -> term MULT term\n"); }
|   term DIV multiplicative-expr        { printf("multiplicative-expr -> term DIV term\n"); }
|   term MOD multiplicative-expr       { printf("multiplicative-expr -> term MOD term\n"); }
;

term:
    var                                 { printf("term -> var\n"); }
|   SUB var %prec NEG                   { printf("term -> var\n"); }
|   NUMBER                              { printf("term -> NUMBER\n"); }
|   SUB NUMBER %prec NEG                { printf("term -> NUMBER\n"); }
|   L_PAREN expression R_PAREN          { printf("term -> L_PAREN expression R_PAREN\n"); }
|   SUB L_PAREN expression R_PAREN %prec NEG  { printf("term -> L_PAREN expression R_PAREN\n"); }
|   ident L_PAREN expressions R_PAREN   { printf("term -> ident L_PAREN expressions R_PAREN\n"); }
;

vars:
    var                                 { printf("vars -> var\n"); }
|   var COMMA vars                      { printf("vars -> var COMMA vars\n"); }
;

var:
    ident                               { printf("var -> ident\n"); }
|   ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET  { printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n"); }
;

%%
int main() {
    yyin = stdin;
    do {
        yyparse();
    } while (!feof(yyin));
}

void yyerror(const char* s) {
    printf("ERROR: %s on line %d", s, yylineno);
    exit(1);
}
