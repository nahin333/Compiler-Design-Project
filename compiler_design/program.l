%{
	#include<stdio.h>
	#include <string.h>
	#include "program.tab.h"
	#include<stdlib.h>
%}

identifiers "nahin_"[a-zA-Z]([a-zA-Z0-9])*
unary_operator "#++"|"#--"|"!#"
library_func "IN<"|"OUT>"
integer [+-]?[0-9]+
float [+-]?[0-9]+[.][0-9]+|[+-]?[.][0-9]+|[1-9]+e[+-][0-9]+
double [+-]?[0-9]+[.][0-9]+{6,12}|[+-]?[.][0-9]+|[1-9]+e[+-][0-9]+{6,12}
char [A-Za-z]+
single_line_comment [/][/].*
multiple_line_comment [ ]*[/][a][/][a-zA-Z0-9!@#*(){}_+-,.:\|?><\n\t ]*[/][a][/]

/*RE and Action*/

%%


{single_line_comment} {
	printf("single line comment\n");

}
{multiple_line_comment} {
	printf("multiple_line_comment");
}

{unary_operator}   {return inc;}
"lb"	 { return LB; }
"rb"	  { return RB; }
"break_loop"    {   return BREAK;     }
"choose"      { return CASE;      }
"switch"      { return SWITCH;	}
"default"     {  return DEFAULT; 	}
"lp_while" { return WHILE;}
"intType"	{ return INT; }
"floatType"	{ return FLOAT; }
"doubleType"	{return DOUBLE;}
"charType"	{ return CHAR; }
":"     { return COL;}
{integer}	{ 
		yylval.number = atoi(yytext);
		return NUM;
	}
"lp_for"   { return FOR;}	
"LP"	{ return LP; }
"RP"	{ return RP; }
"cm"	{ return CM; }
"##"	{ return SM; }
"plus"	{ return PLUS;}
"minus"	{ return MINUS; }
"multiply"	{ return MULT; }
"divide"	{ return DIV; }
"pow"	{ return POW; }
"fact"	{ return FACT; }
"eq"		{ return ASSIGN;  }
"greater"	{ return GT; }
"to" {return TO;}
"less"	{ return LT; }
"lessoreq" {return LOE;}
"greateroreq" {return GOT;}

{identifiers} {
	 yylval.string=strdup(yytext);
	 return VAR;
	}

"if"		{ return IF;}
"else"		{ return ELSE;}
"else_if"	{ return ELIF; }

"function "[A-Za-z]+	{ return FUNCTION; }

[ \t\n]*	

.	{
		yyerror("Unknown Character.\n");
	}
%%

int main()
{
	yyin = freopen("in.txt","r",stdin);
	yyout = freopen("out.txt","w",stdout);
	yyparse();	
}