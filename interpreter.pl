/***
An Interpreter for Assignment 3 on PROP HT2014 at DSV/SU.
Second grammar, grade A/B
Loric Brevet
Etienne Cuvillon
***/

:- [tokenizer].

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut),
	output_result(OutputFile,ParseTree,VariablesOut).

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

parse(ParseTree) --> block(ParseTree).

block(block(Left_curly,Stmts,Right_curly)) -->
	left_curly(Left_curly), statements(Stmts), right_curly(Right_curly).

statements(statements(Assign,Stmts)) -->
	assignment(Assign), !, statements(Stmts).
statements(statements) --> !.

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
factor(factor(Ident)) -->
	ident(Ident).
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
left_curly(left_curly) --> ['{'].
right_curly(right_curly) --> ['}'].
semicolon(semicolon) --> [;].

/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	TODO: Implement an evaluate predicate that evaluates a parse-tree and
	returns the state of the program after evaluation as a list of variables and
	their values.
***/

evaluate(ParseTree,VariablesIn,VariablesOut):-
	ParseTree = block(left_curly,Stmts,right_curly),
	evaluate_stmts(Stmts,VariablesIn,VariablesOut).

evaluate_stmts(statements,Variables,Variables).
evaluate_stmts(statements(Assign,Stmts),VariablesIn,VariablesOut):-
	evaluate_assign(Assign,VariablesIn,VariablesNew),
	evaluate_stmts(Stmts,VariablesNew,VariablesOut).

evaluate_assign(Assign,VariablesIn,VariablesNew):-
	Assign = assignment(ident(Id),assign_op,Expr,semicolon),
	evaluate_expr(Expr,nil,0,Res,VariablesIn),
	add_list(Id = Res,VariablesIn,VariablesNew).

evaluate_expr(expression(Term),Op,Prec,Res,VariablesIn):-
	evaluate_term(Term,nil,0,Res2,VariablesIn),
	eval(Prec,Res2,Op,Res).
evaluate_expr(expression(Term,Op1,Expr),Op2,Prec,Res,VariablesIn):-
	evaluate_term(Term,nil,0,Res2,VariablesIn),
	evaluate_expr(Expr,Op1,Res3,Res,VariablesIn),
	eval(Prec,Res2,Op2,Res3).

evaluate_term(term(Factor),Op,Prec,Res,VariablesIn):-
	evaluate_factor(Factor,Res2,VariablesIn),
	eval(Prec,Res2,Op,Res).
evaluate_term(term(Factor,Op1,Term),Op2,Prec,Res,VariablesIn):-
	evaluate_factor(Factor,Res2,VariablesIn),
	evaluate_term(Term,Op1,Res3,Res,VariablesIn),
	eval(Prec,Res2,Op2,Res3).

evaluate_factor(factor(int(Int)),Int,_).
evaluate_factor(factor(ident(Ident)),Res,VariablesIn):-
	find(VariablesIn,Ident,Res).
evaluate_factor(factor(left_paren,Expr,right_paren), Res,VariablesIn):-
	evaluate_expr(Expr,nil,0,Res,VariablesIn).

eval(X,Y,add_op,X+Y).
eval(X,Y,sub_op,X-Y).
eval(X,Y,mult_op,X*Y).
eval(X,Y,div_op,X/Y).
eval(_,Y,nil,Y).

/*
add_list(+Elem, +List1, -List2)
List2 is List1 with Elem added in.
*/
add_list(Elem, List1, [Elem|List1]).

/*
find(+Vars,+Ident,-Value)
Vars is a list of variables. If the variable Ident is in Vars then Value
is the value of Ident (Ident = Value), otherwise Ident = 0.
*/
find([],_,0).
find([Id = Value|_],Id,Value):-!.
find([Id1 = _|Y],Id2,Res):-
	Id1 \== Id2,
	find(Y,Id2,Res).
