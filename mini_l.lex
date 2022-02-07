%option noyywrap
%option yylineno

%{
int line = 1, column = 1;
%}

digit               [0-9]
letter              [a-zA-Z]

%%
"function"          column += yyleng; printf("FUNCTION\n");
"integer"           column += yyleng; printf("INTEGER\n");
"array"             column += yyleng; printf("ARRAY\n");
"of"                column += yyleng; printf("OF\n");
"read"              column += yyleng; printf("READ\n");
"write"             column += yyleng; printf("WRITE\n");
"return"            column += yyleng; printf("RETURN\n");
"beginparams"       column += yyleng; printf("BEGIN_PARAMS\n");
"endparams"         column += yyleng; printf("END_PARAMS\n");
"beginlocals"       column += yyleng; printf("BEGIN_LOCALS\n");
"endlocals"         column += yyleng; printf("END_LOCALS\n");
"beginbody"         column += yyleng; printf("BEGIN_BODY\n");
"endbody"           column += yyleng; printf("END_BODY\n");
"beginloop"         column += yyleng; printf("BEGINLOOP\n");
"endloop"           column += yyleng; printf("ENDLOOP\n");
"if"                column += yyleng; printf("IF\n");
"then"              column += yyleng; printf("THEN\n");
"endif"             column += yyleng; printf("ENDIF\n");
"else"              column += yyleng; printf("ELSE\n");
"while"             column += yyleng; printf("WHILE\n");
"do"                column += yyleng; printf("DO\n");
"continue"          column += yyleng; printf("CONTINUE\n");
"and"               column += yyleng; printf("AND\n");
"or"                column += yyleng; printf("OR\n");
"not"               column += yyleng; printf("NOT\n");
"true"              column += yyleng; printf("TRUE\n");
"false"             column += yyleng; printf("FALSE\n");
"-"                 column += yyleng; printf("SUB\n");
"+"                 column += yyleng; printf("ADD\n");
"*"                 column += yyleng; printf("MULT\n");
"/"                 column += yyleng; printf("DIV\n");
"%"                 column += yyleng; printf("MOD\n");
"=="                column += yyleng; printf("EQ\n");
"<>"                column += yyleng; printf("NEQ\n");
"<"                 column += yyleng; printf("LT\n");
">"                 column += yyleng; printf("GT\n");
"<="                column += yyleng; printf("LTE\n");
">="                column += yyleng; printf("GTE\n");
";"                 column += yyleng; printf("SEMICOLON\n");
":"                 column += yyleng; printf("COLON\n");
","                 column += yyleng; printf("COMMA\n");
"("                 column += yyleng; printf("L_PAREN\n");
")"                 column += yyleng; printf("R_PAREN\n");
"["                 column += yyleng; printf("L_SQUARE_BRACKET\n");
"]"                 column += yyleng; printf("R_SQUARE_BRACKET\n");
":="                column += yyleng; printf("ASSIGN\n");

##(.)*\n            line++; column = 1;

[ \t]+              column += yyleng;

"\n"                line++; column = 1;

{digit}+            column += yyleng; printf("NUMBER %s\n", yytext);

({letter})|({letter}({letter}|{digit}|"_")*({letter}|{digit})) { column += yyleng; printf("IDENT %s\n", yytext); }

((("_")+)|(({digit})+({letter}|"_")))({digit}|{letter}|"_")* { printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", line, column, yytext); exit(1); }

({letter})({digit}|{letter}|"_")*("_") { printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", line, column, yytext); exit(1); }

.                   { printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", line, column, yytext); exit(1); }
%%

int main() { yylex(); }