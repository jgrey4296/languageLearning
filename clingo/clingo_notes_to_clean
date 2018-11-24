%--------------------
%Language: Answer Set Programming
%--------------------

%-------------------- Links
%www.amazon.com/Answer-Solving-Practice-Martin-Gebser/dp/1608459713
%http://potassco.sourceforge.net/teaching.html


%-------------------- Inspection and Output:

%-------------------- Good Style
%gringo expects rules to be safe
%all variables that appear in a rule have to appear in some positive literal
%in the body, thus binding the variable to some existing constant.

%only atoms appearing in some head can appear in answer sets.

%there are no answer sets that satisfy all literals an integrity constraint. 
%thus you are defining situations you don't want.

%-------------------- Compilation
%clingo <file>

%-------------------- Imports

%-------------------- Comments
%
%-------------------- Whitespace and Statements
%End statements with a full stop.

%-------------------- Memory Management

%-------------------- Value vs Reference 

%-------------------- Scope

%-------------------- Terms:
%Simple Terms: integer, constant, variable, _, #supremum, #infimum

% Constants: _[a-z][A-Za-z0-9]

% Variables: _[A-Z][A-Za-z0-9]

% Anonymous Variable: _

% Function: 
%           constant ( simpleterm (, simpleterm)+ )
bird(1,a,X).

% Terms: [simpleterms,functions]

% Rule A0:- L1,... ,Ln. 
%      Head :- Body.

% Fact: A0. 
%      Head

% Integrity Constraint: :- L1,...,Ln.
%                         Just Body.

% Default Negation: assumed to hold unless A is derived
not a.

% Classical Negation: holds if the explicit negation is derived.
-a.

% Cross :- not Train. //cross when you don't know there is a train coming
% vs
% Cross :- -Train. //cross when you know there is no train coming.

%AND: ,
a, b.

%OR: TRY NOT TO USE THIS
a | b.


%Conditions ':' provide OR on lsh, AND on rhs.
meet :- available(X) : person(X).
on(X) : day(X) :- meet.

%becomes:
meet :- available(jane), available(john).
on(mon) | on(tue) | on(wed) | on(thu) | on(fri) :- meet.


%-------------------- Arithmetic
%place inside functions and have the arguments on the rhs:
left(4). right(6).
plus(L + R) :- left(L), right(R).


%available: + - * / #div \ #mod |- #abs ** #pow

% Note that it is important that variables in the scope of an
%    arithmetic function are not bound by a corresponding atom. For
%    instance, the rule p(X) :- p(X+1). is not safe but p(X-1) :-
%    p(X). is. Although, the latter might produce an infinite grounding and
%    gringo not necessarily halts when given such an input.

%    having plus(L + R) :- left(L - 1), right(R).  this will essentially
%    add 1 to L before adding L + R, as left(L) will match to 7, but L-1
%    needs to match to 7 to, so L needs to be 8
 

%-------------------- Logical Operations:
% And, Or, XOr, Neg:
% &    ?   ^    ~

%-------------------- Comparision: 
%   The following built-in predicates
%   permit term comparisons within the bodies of rules: == (equal), !=
%   (not equal), < (less than), <= (less than or equal), > (greater
%   than), >= (greater than or equal).


%-------------------- Assignment:
%= in the body of a rule to create 
%a synthetic variable on the lhs.
%despite what it says in the guide, := does not work.


%-------------------- Constraints:
%three to size courses should be enrolled
3 { enroll(C) : course(C,_,_) } 6.

%-------------------- Examples:
first(1;2).

%combine parameters into new fact:
second(X,Y) :- first(X), first(Y), X < Y.

%synthesise new parameters:
third(XX) :- XX = X + Y, second(X,Y).

%place entire fact inside another:
fourth(X) :- X = third(Y), third(Y).

%extract a fact from inside another:
fifth(X) :- X = Y, fourth(Y).

%extract in more detail:
sixth(X) :- X = Y, third(Y) = H ,fifth(H).


%---------------------------------------- Language Specific Concepts:
%Define a fact
peg(a).

%Define a set of facts:
peg(a;b;c).

%Define a numeric sequence of facts:
disk(1..4).

%Generate, Define, Test phases


