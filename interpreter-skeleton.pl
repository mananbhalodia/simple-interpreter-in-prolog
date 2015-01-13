/***
A skeleton for Assignment 3 on PROP HT2014 at DSV/SU.
Peter Idestam-Almquist, 2014-12-23.
***/

:- [tokenizer].

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	/*evaluate(ParseTree,[],VariablesOut),*/
	output_result(OutputFile,ParseTree,_).

output_result(OutputFile,ParseTree,Variables):-
	open(OutputFile,write,OutputStream),
	write(OutputStream,'PARSE TREE:'),
	nl(OutputStream),
	writeln_term(OutputStream,0,ParseTree),
	nl(OutputStream),
	write(OutputStream,'EVALUATION:'),
	nl(OutputStream),
	write_list(OutputStream,Variables),
	close(OutputStream).

writeln_term(Stream,Tabs,int(X)):-
	write_tabs(Stream,Tabs),
	writeln(Stream,int(X)).
writeln_term(Stream,Tabs,ident(X)):-
	write_tabs(Stream,Tabs),
	writeln(Stream,ident(X)).
writeln_term(Stream,Tabs,Term):-
	functor(Term,_Functor,0), !,
	write_tabs(Stream,Tabs),
	writeln(Stream,Term).
writeln_term(Stream,Tabs1,Term):-
	functor(Term,Functor,Arity),
	write_tabs(Stream,Tabs1),
	writeln(Stream,Functor),
	Tabs2 is Tabs1 + 1,
	writeln_args(Stream,Tabs2,Term,1,Arity).

writeln_args(Stream,Tabs,Term,N,N):-
	arg(N,Term,Arg),
	writeln_term(Stream,Tabs,Arg).
writeln_args(Stream,Tabs,Term,N1,M):-
	arg(N1,Term,Arg),
	writeln_term(Stream,Tabs,Arg),
	N2 is N1 + 1,
	writeln_args(Stream,Tabs,Term,N2,M).

write_tabs(_,0).
write_tabs(Stream,Num1):-
	write(Stream,'\t'),
	Num2 is Num1 - 1,
	write_tabs(Stream,Num2).

writeln(Stream,Term):-
	write(Stream,Term),
	nl(Stream).

write_list(_Stream,[]).
write_list(Stream,[Ident = Value|Vars]):-
	write(Stream,Ident),
	write(Stream,' = '),
	format(Stream,'~1f',Value),
	nl(Stream),
	write_list(Stream,Vars).

/***
parse(-ParseTree)-->
	TODO: Implement a definite clause grammar defining our programming language,
	and returning a parse tree.
***/

parse(ParseTree) --> assignment(ParseTree).
/*
block(block(Left_curly,Stmts,Right_curly)) -->
	left_curly(Left_curly), statements(Stmts), right_curly(Right_curly).

statements(statements(Assign,Stmts)) -->
	assignment(Assign), statements(Stmts).
*/
assignment(assignment(Id,Eq,Expr,Sem)) -->
	ident(Id), assign_op(Eq), expression(Expr), semicolon(Sem).

expression(expression(Term)) -->
	term(Term).
expression(expression(Term,Add_op,Expr)) -->
	term(Term), add_op(Add_op), expression(Expr).
expression(expression(Term,Sub_op,Expr)) -->
	term(Term), sub_op(Sub_op), expression(Expr).

term(term(Factor)) -->
	factor(Factor).
term(term(Factor,Mult_op,Term)) -->
	factor(Factor), mult_op(Mult_op), term(Term).
term(term(Factor,Div_op,Term)) -->
	factor(Factor), div_op(Div_op), term(Term).

factor(factor(Int)) -->
	int(Int).
factor(factor(Left_paren, Expr, Right_paren)) -->
	left_paren(Left_paren), expression(Expr), right_paren(Right_paren).

ident(ident(Ident)) -->	[Ident],{atom(Ident)}.
int(int(Int)) --> [Int],{integer(Int)}.
assign_op(assign_op) --> [=].
add_op(add_op) --> [+].
sub_op(sub_op) --> [-].
mult_op(mult_op) --> [*].
div_op(div_op) --> [/].
left_paren(left_paren) --> ['('].
right_paren(right_paren) --> [')'].
/*left_curly(left_curly) --> ['{'].
right_curly(right_curly) --> ['}'].*/
semicolon(semicolon) --> [;].

/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	TODO: Implement an evaluate predicate that evaluates a parse-tree and
	returns the state of the program after evaluation as a list of variables and
	their values.
***/

