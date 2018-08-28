% Jiahui Wang
% 5171973
% Assignment 1 - Prolog Programming

%Q1
% Write a predicate sumsq_neg(Numbers, Sum) that sums the squares of only the negative numbers in a list of numbers.
% Comment:
% As new to prolog, I just used a naive way to solve this question
% Because Q1 is pretty stright forward.
% First two functions are just used to identify whether the number is positive
% or negative.
% Then only calculate the square of the negative numbers.
is_larger_0(Num) :-
    Num > 0.
is_smaller_0(Num) :-
    Num < 0.
sq(Num, Value) :-
    Value = Num * Num.
sumsq_neg([], 0).
sumsq_neg([Head|Rest], Sum) :-
    is_larger_0(Head),
    sumsq_neg(Rest, Sum).
sumsq_neg([Head|Rest], Sum) :-
	is_smaller_0(Head),
	sumsq_neg(Rest, Temp),
	sq(Head, Sq),
    Sum is Sq + Temp.

%Q2
% Write a predicate all_like_all(Who_List, What_List) that takes
% a list of people Who_List and a list of items What_List.
% Succeed if every person in Who_List likes every item in What_List,
% according to the predicate likes(Who, What).
% Succeed if either Who_List or What_List is empty.
% Comment:
% For this question, I think handling base cases are quite important.
% This a naive but pretty stright forward way to solve the question.
% The two recursives check every possible combinations from "likes".
all_like_all(_,[]).
all_like_all([],_).
all_like_all([],[]).
all_like_all([Person|Tail2], [What|Tail1]) :-
    likes(Person, What),
    all_like_all([Person|Tail2], Tail1),
    all_like_all(Tail2, [What|Tail1]).

%Q3
% Write a predicate sqrt_table(N, M, Result) that
% binds Result to the list of pairs consisting of a number and its square root,
% from N down to M, where N and M are non-negative integers, and N >= M. For example:
% Comment:
% There are two cases: one is N > M, the other one is N = M.
% The second "sqrt_table" will check if N = M, but accually,
% it checks Equality between M and every elements which are larger than M
% BUT, it will create some redundant operations (depends on how big the N and M are).
% The solution would be adding a
% "cut", which is a "!" at the end of the second "sqrt_table".
% Since our testing numbers are not that large, I would not use a cut.
sqrt_table(N, M, [[N, Sqrt] | Result]) :-
    Sqrt is sqrt(N),
    Next_N is N - 1,
    N > M,
    sqrt_table(Next_N, M, Result).
sqrt_table(N, M, [[N, Result]]) :-
    N =:= M,
    Result is sqrt(N).

%Q4
% Write a predicate chop_up(List, NewList) that takes List and binds NewList to
% List with all sequences of successive increasing whole numbers
% replaced by a two-item list containing only the first and last number in the sequence
% Comment:
% This question is a bit difficult.
% I tried to use one recursive to solve this question,
% but I failed to find the current answer.
% I used two recursives to solve this question.
% Also a "cut" was used to prevent the wrong output(stop the program finding the wrong output).
% The main idea is to find the min and max of consecutive numbers.
chop_up([], []).
chop_up([Element], [Element]).
% chop head
chop_up([Element1|[Element2|Tail]], [Head|Result]) :-
    Element2 =\= Element1 + 1,
    Head = Element1,
    chop_up([Element2|Tail], Result).
% ground Min and keep find consecutive numbers
chop_up([Min|[Element|Tail]], [[Min, Max]|Result]) :-
    Min is Element - 1,
    chop_up1(Min, [Element|Tail], [[Min, Max]|Result]).
% in the middle of more than two consecutive whole numbers
chop_up1(Prev, [Element|Tail], Result) :-
    Element =:= Prev + 1,
    chop_up1(Element, Tail, Result),
    !.
% ground Max and move on
chop_up1(Prev, [Element|Tail], [[_, Max]|Result]) :-
    Element =:= Prev + 1,
    Max = Element,
    chop_up(Tail, Result).

%Q5
% Binary expression-trees whose leaves are either of the form tree(empty, Num, empty), Num is a number,
% or tree(empty, z, empty) in which case we will think of the letter z as a kind of "variable".
% Every tree is either a leaf or of the form tree(L, Op, R) where L and R are the left and right subtrees.
% Op is one of the arithmetic operators '+', '-', '*', '/' (signifying addition, subtraction, multiplication and division).
% Comment:
% This question looks difficult, but it is quite stright forward.
% All the key points to solve this question are given in the question.
% The only thing I need to be careful is Num != z.
tree_eval(_, tree(empty, Num, empty), Num):-
    dif(Num,z).
tree_eval(Value, tree(empty, z, empty), Value).
tree_eval(Value, tree(L, Op, R), Eval) :-
	tree_eval(Value, L, Left),
	tree_eval(Value, R, Right),
	Expression =.. [Op, Left, Right],
	Eval is Expression.
