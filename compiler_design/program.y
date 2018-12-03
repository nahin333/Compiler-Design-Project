%{
	#include<stdio.h>
	#include <math.h>
	int cnt=1,cntt=1,val,my_val,track=0;
	typedef struct entry {
    	char *str;
    	int n;
	}storage;
	storage store[1000],sym[1000];
	void insert (storage *p, char *s, int n);
	int cnt2=1; 
	#define pi  3.1416
	
%}
%union 
{
        int number;
        char *string;
}
/* BISON Declarations */

%token <number> NUM
%token <string> VAR 
%token <string> IF ELIF ELSE FUNCTION INT FLOAT DOUBLE CHAR LP RP LB RB CM SM PLUS MINUS MULT DIV POW FACT ASSIGN FOR COL WHILE BREAK COLON DEFAULT CASE SWITCH inc LOGIC LOE GOT TO
%type <string> statement
%type <number> expression
%type <number> expression_switch
%nonassoc IFX
%nonassoc ELSEX
%left LT GT
%left PLUS MINUS
%left MULT DIV
%left FACT
%right POW

%%

program: FUNCTION LP RP LB cstatement RB { printf("\nSuccessful compilation\n"); }
	 ;

cstatement: /* empty */

	| cstatement statement
	
	| cdeclaration
	;

cdeclaration:	TYPE ID1 SM	{ printf("\nvalid declaration\n"); }
   
			;
			
TYPE : INT

     | FLOAT

     | CHAR
     ;

ID1  : ID1 CM VAR	{
						if(number_for_key($3))
						{
							printf("%s is already declared\n", $3 );
						}
						else
						{
							
							insert(&store[cnt],$3, cnt);
							cnt++;
							
						}
			}

     |VAR	{
				if(number_for_key($1))
				{
					printf("%s is already declared\n", $1 );
				}
				else
				{
					insert(&store[cnt],$1, cnt);
							cnt++;
				}
			}
     ;

statement: SM
	| SWITCH LP expression_switch RP LB BASE RB    {printf("SWITCH case.\n");val=$3;} 

	| expression SM 			{ printf("\nvalue of expression: %d\n", ($1)); }

        | VAR ASSIGN expression SM 		{
							if(number_for_key($1)){
								int i = number_for_key2($1);
								if (!i){
									insert(&sym[cntt], $1, $3);
									printf("\n(%s) Value of the variable: %d\t\n",$1,$3);
									cntt++;
								}else{
									sym[i].n = $3;
									printf("\n(%s) Value of the variable: %d\t\n",$1,$3);
								}
							}
							else {
								printf("%s not declared yet\n",$1);
							}
							
						}

	| IF LP expression RP LB statement RB %prec IFX {
								if($3)
								{
									printf("\nWe are  in IF and the value is: %d\n",($6));
								}
								else
								{
									printf("\ncondition value zero in IF block\n");
								}
							}

	| IF LP expression RP LB statement RB ELSE LB statement RB %prec ELSEX{
								 	if($3)
									{
										printf("\nWe are in IF: %d\n",$6);
									}
									else
									{
										printf("\nWe are in ELSE and the value is : %d\n",$10);
									}
								   }							   
	| FOR LP NUM COL NUM RP LB expression RB     {
	   int i=0;
	   for(i=$3;i<=$5;i++){
	   printf("for loop statement\n");
	   }
	}
	| WHILE LP NUM TO NUM RP LB expression RB   {
										int i;
										printf("While LOOP: ");
										for(i=$3;i<=$5;i++)
										{
											printf("%d ",i);
										}
										printf("\n");
										printf("value of the expression: %d\n",$8);

	}
	;
	
			BASE : Bas   
				 | Bas Dflt 
				 ;

			Bas   : /*NULL*/
				 | Bas Cs     
				 ;

			Cs    : CASE NUM COL expression SM   {
								if (my_val == $2){
									track = 1;
									printf("\nCase No : %d  and Result :  %d\n",$2,$4);
								}							  
							  		
					}
				 ;

			Dflt    : DEFAULT COL NUM SM    {					
							if (track != 1){
								printf("\nResult in default Value is :  %d \n",$3);
							}
							track = 0;				
					}
				 ;    
expression_switch:	NUM				{ $$ = $1;	my_val = $1;}

	| VAR				{  
	                       if(number_for_key($1))
	                       {
	                       int i = number_for_key2($1);  $$ = sym[i].n; printf("Variable value: %d",$$)};
	                       my_val =$1; 
	                       } 
	                       

	| expression_switch PLUS expression_switch	{ $$ = $1 + $3; my_val = $$; }

	| expression_switch MINUS expression_switch	{ $$ = $1 - $3; my_val = $$; }


	| expression_switch MULT expression_switch	{ $$ = $1 * $3; my_val = $$; }

	| expression_switch DIV expression_switch	{ 	if($3) 
				  		{
				     			$$ = $1 / $3;
				     			my_val = $$;
				  		}
				  		else
				  		{
							$$ = 0;
							printf("\ndivision by zero\t");
							my_val = $$;
				  		} 	
				    	}
	| expression_switch POW expression_switch { $$ = pow($1,$3);my_val = $$; }

	| expression_switch FACT {
						int mult=1 ,i;
						for(i=$1;i>0;i--)
						{
							mult=mult*i;
						}
						$$=mult;
						my_val = $$;
						
					 }	
;
expression: NUM				{ $$ = $1;	}

	| VAR				{  
	                       if(number_for_key($1))
	                       {
	                       int i = number_for_key2($1);  $$ = sym[i].n; printf("Variable value: %d",$$)};

	                       } 

	| expression PLUS expression	{ $$ = $1 + $3; }

	| expression MINUS expression	{ $$ = $1 - $3; }

	| expression MULT expression	{ $$ = $1 * $3; }

	| expression DIV expression	{ 	if($3) 
				  		{
				     			$$ = $1 / $3;
				  		}
				  		else
				  		{
							$$ = 0;
							printf("\ndivision by zero\t");
				  		} 	
				    	}
	| expression POW expression { $$ = pow($1,$3); }

	| expression FACT {
						int mult=1 ,i;
						for(i=$1;i>0;i--)
						{
							mult=mult*i;
						}
						$$=mult;
						
					 }	

	| expression LT expression	{ $$ = $1 < $3; }

	| expression GT expression	{ $$ = $1 > $3; }

	| LP expression RP		{ $$ = $2;	}
	
	| inc expression inc         { $$=$2+1; printf("inc: %d\n",$$);}
	;
%%

void insert(storage *p, char *s, int n)
{
  p->str = s;
  p->n = n;
}

int number_for_key(char *key)
{
    int i = 1;
    char *name = store[i].str;
    while (name) {
        if (strcmp(name, key) == 0)
            return i;
        name = store[++i].str;
    }
    return 0;
}

int number_for_key2(char *key)
{
    int i = 1;
    char *name = sym[i].str;
    while (name) {
        if (strcmp(name, key) == 0)
            return i;
        name = sym[++i].str;
    }
    return 0;
}

int yywrap()
{
return 1;
}

yyerror(char *s){
	printf( "%s\n", s);
}