--------------------
Language: Prolog / ANSProlog
--------------------

-------------------- Links

-------------------- Inspection and Output:

-------------------- Good Style

-------------------- Compilation

-------------------- Imports

-------------------- Comments


-------------------- Whitespace and Statements


-------------------- Memory Management

-------------------- Value vs Reference 


-------------------- Scope

---------------------------------------- Basic Data Types

-------------------- Arithmetic

-------------------- Bitwise

-------------------- Strings

-------------------- Booleans

-------------------- Comparisons and Logic


-------------------- Variables


---------------------------------------- Data Structures

-------------------- Arrays

-------------------- Objects

---------- Object Creation:


-------------------- Constructors

---------------------------------------- Control Structures

//IF
if(true || false){};

//WHILE
while(true){};
do {} while(true);

//FOR
for(var i = 0; i < 5; i++){}

for(var x in a){
    if(a.hasOwnProperty(x)){

    }
}

//Switch:
//checks for equality with ===
//uses break;
switch(something){
    case 'A':
    2+2;
    break;
}

------------------------------ Functions


------------------------------ Examples



---------------------------------------- Language Specific Concepts:

--------------------
prolog
--------------------
http://www.cs.oswego.edu/~odendahl/coursework/notes/prolog/synopsis/con.html
http://www.swi-prolog.org/

binary: swipl
quit: ?- halt.

/* comments */


commands: 
consult(user|<filename>)
assert(<term>)
retract(<term>)

Procedure View:
fac(0,1).
fac(N,X) :- N > 0, M is N - 1, fac(M,Y), X is Y * N.

Logic View:
Theorems: good(life).
Axioms: worthwhile(X) :- good(X).
and queries: 
|?- good(life).
|?- good(X)


Main Language elements:
'an atom'
+ - :- < >
32
"a string"
_avar
Adifferentvar
[a, b | c]

