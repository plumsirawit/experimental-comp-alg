/*
  An attempt to compute rational maps.
*/

/*
  eval(Term, Out): evaluate Term at [0, 1, 0] and return Out.
*/

eval(Num, Out) :- number(Num), Out = Num.
eval(x, Out) :- Out = 0.
eval(y, Out) :- Out = 1.
eval(z, Out) :- Out = 0.
eval(Term, Out) :- Term = T1 + T2, eval(T1, Out1), eval(T2, Out2), Out = Out1+Out2.
eval(Term, Out) :- Term = T1 * T2, eval(T1, Out1), eval(T2, Out2), Out = Out1*Out2.
eval(Term, Out) :- Term = T1 / T2, eval(T1, Out1), eval(T2, Out2), Out = Out1/Out2.

/*
  simplify(Term, Out): simplify numerical Term to a single numeric value.
*/

simplify(Num, Out) :- number(Num), Out = Num.
simplify(0+D, E) :- simplify(D, E).
simplify(D+0, E) :- simplify(D, E).
simplify(1*D, E) :- simplify(D, E).
simplify(D*1, E) :- simplify(D, E).
simplify(0*_D, E) :- E = 0.
simplify(_D*0, E) :- E = 0.
simplify(_D/0, E) :- E = oo.
simplify(0/_D, E) :- E = 0.
simplify(D+F, E) :- D \= 0, F \= 0, simplify(D, E1), simplify(F, E2), ((D = E1, F = E2, E = E1 + E2); (dif(D, E1); dif(F, E2)), simplify(E1+E2, E)).
simplify(D*F, E) :- D \= 0, F \= 0, D \= 1, F \= 1, simplify(D, E1), simplify(F, E2), ((D = E1, F = E2, E = E1 * E2); (dif(D, E1); dif(F, E2)), simplify(E1*E2, E)).
simplify(D-F, E) :- simplify(D, E1), simplify(F, E2), ((D = E1, F = E2, E = E1 - E2); (dif(D, E1); dif(F, E2)), simplify(E1-E2, E)).
simplify(-D, E) :- simplify(D, E1), E = -E1.

/*
  eval_zero(Term): check whether Term evaluates to zero or not.
*/

eval_zero(Term) :- eval(Term, Out), simplify(Out, Out2), 0 is Out2.

/*
  needle_haystack(N, H): check whether an atom N exists in H. (https://stackoverflow.com/a/57602983/8757529)
*/
needle_haystack(N,H) :- N==H.
needle_haystack(N,H) :- H=..[_|As],member(A,As),needle_haystack(N,A).

/*
  eval_proj(TermX, TermY, TermZ, OutX, OutY, OutZ): evaluate [TermX; TermY; TermZ] (projective coordinates) into [OutX; OutY; OutZ].
*/

eval_proj(TermX, TermY, TermZ, OutX, OutY, OutZ) :- 
  number(TermX),
  number(TermY),
  number(TermZ),
  OutX is TermX,
  OutY is TermY,
  OutZ is TermZ.

eval_proj(TermX, TermY, TermZ, OutX, OutY, OutZ) :-
  TermX = Lambda * SubtermX,
  TermY = Lambda * SubtermY,
  TermZ = Lambda * SubtermZ,
  eval_proj(SubtermX, SubtermY, SubtermZ, OutX, OutY, OutZ).

eval_proj(TermX, TermY, TermZ, OutX, OutY, OutZ) :-
  eval(TermX, InterX),
  eval(TermY, InterY),
  eval(TermZ, InterZ),
  simplify(InterX, Inter2X),
  simplify(InterY, Inter2Y),
  simplify(InterZ, Inter2Z),
  eval_proj(Inter2X, Inter2Y, Inter2Z, OutX, OutY, OutZ).


/*ratmap(TermX, TermY, TermZ) :-*/