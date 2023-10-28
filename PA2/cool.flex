%{ /*declarations*/
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */
// extern "C" int yylex(); /* remove */

#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

static int comment_lines = 0;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

bool check_strlen();
int strlen_err();




%}
/*definitions*/
%option noyywrap

%x              COMMENT
%x              STRING

DIGIT           [0-9]
INT             {DIGIT}+
NEWLINE         (\r\n|\n)+
LETTER          [a-zA-z]
WHITESPACE      [ \t]*
INLINECOMMENT   --.*\n

CLASS           (?i:class)
ELSE            (?i:else)
IF              (?i:if)
FI              (?i:fi)
IN              (?i:in)
INHERITS        (?i:inherits)
LET             (?i:let)
LOOP            (?i:loop)
POOL            (?i:pool)
THEN            (?i:then)
WHILE           (?i:while)
CASE            (?i:case)
NEW             (?i:new)
ISVOID          (?i:isviod)
OF              (?i:of)
NOT             (?i:not)

TYPEID [A-Z]({DIGIT}|{LETTER})*
OBJECTID [a-z]({DIGIT}|{LETTER})*

%% /*rules*/

"(*" {
  BEGIN(COMMENT);
}
"*)" {
  cool_yylval.error_msg = "Unmatched *)";
  return ERROR;
}
<COMMENT><<EOF>> {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "EOF in comment";
  return ERROR;
}
<COMMENT>\n {
  curr_lineno++;
}
<COMMENT>. {
  /* pass, nothing to do in comment*/
}
<COMMENT>"*)" {
  BEGIN(INITIAL);
}
{INLINECOMMENT} {
  curr_lineno++;
}

\" {
  BEGIN(STRING);
  string_buf_ptr = string_buf;
}
<STRING>\" {
  BEGIN(INITIAL);
  if (check_strlen()) return strlen_err();
  string_buf_ptr = 0;
  cool_yylval.symbol = stringtable.add_string(string_buf);
  return STR_CONST;
}
<STRING><<EOF>> {
  cool_yylval.error_msg="EOF in string constant";
  return ERROR;
}
<STRING>\\\n {
  curr_lineno++;
}
<STRING>\n {
  curr_lineno++;
  BEGIN(INITIAL);
  cool_yylval.error_msg = "Unterminated string constant";
  return ERROR;
}
<STRING>\\[^ntbf] {
  if (check_strlen()) return strlen_err();
  *string_buf_ptr++ = yytext[1];
}
<STRING>\\[n] {
  if (check_strlen()) return strlen_err();
  *string_buf_ptr++ = '\n';
}
<STRING>\\[t] {
  if (check_strlen()) return strlen_err();
  *string_buf_ptr++ = '\t';
}
<STRING>\\[b] {
  if (check_strlen()) return strlen_err();
  *string_buf_ptr++ = '\b';
}
<STRING>\\[f] {
  if (check_strlen()) return strlen_err();
  *string_buf_ptr++ = '\f';
}
<STRING>. {
  if (check_strlen()) return strlen_err();
  *string_buf_ptr++ = *yytext;
}

{INT} {
  cool_yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}
"false" {
  cool_yylval.boolean = false;
  return BOOL_CONST;
}
"true" {
  cool_yylval.boolean = true;
  return BOOL_CONST;
}

"=>"        { return DARROW; }
"=<"        { return LE; }
"<-"        { return ASSIGN; }
"<"         { return '<'; }
"@"         { return '@'; }
"~"         { return '~'; }
"="         { return '='; }
"."         { return '.'; }
"-"         { return '-'; }
","         { return ','; }
"+"         { return '+'; }
"*"         { return '*'; }
"/"         { return '/'; }
"}"         { return '}'; }
"{"         { return '{'; }
"("         { return '('; }
")"         { return ')'; }
":"         { return ':'; }
";"         { return ';'; }
{CLASS}     { return CLASS; }
{ELSE}      { return ELSE; }
{FI}        { return FI; }
{IF}        { return IF; }
{IN}        { return IN; }
{INHERITS}  { return INHERITS; }    
{LET}       { return LET; } 
{LOOP}      { return LOOP; }    
{POOL}      { return POOL; }
{THEN}      { return THEN; }
{WHILE}     { return WHILE; }
{CASE}      { return CASE; }
{NEW}       { return NEW; }
{OF}        { return OF; }
{NOT}       { return NOT; }

{OBJECTID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}
{TYPEID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}
\n { curr_lineno++; }
{WHITESPACE} {
  /* pass */
}
. {
  /*error*/
  cool_yylval.error_msg = strdup(yytext);
  return ERROR;
}

%% /* user subroutines*/

bool check_strlen() {
  return string_buf_ptr - string_buf + 1 > MAX_STR_CONST;
}

int strlen_err() {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "String exceeds maximum size";
  return ERROR;
}