/*
  An attempt to compute rational maps.
*/

:- use_module(library(clpq)).

/*
  eval(Term, X, Y, Z, Out): evaluate Term at [X, Y, Z] and return Out.
*/

eval(Num, _X, _Y, _Z, Out) :- number(Num), {Out = Num}.
eval(x, X, _Y, _Z, Out) :- {Out = X}.
eval(y, _X, Y, _Z, Out) :- {Out = Y}.
eval(z, _X, _Y, Z, Out) :- {Out = Z}.
eval(Term, X, Y, Z, Out) :- Term = T1 + T2, eval(T1, X, Y, Z, Out1), eval(T2, X, Y, Z, Out2), {Out = Out1+Out2}.
eval(Term, X, Y, Z, Out) :- Term = T1 * T2, eval(T1, X, Y, Z, Out1), eval(T2, X, Y, Z, Out2), {Out = Out1*Out2}.
eval(Term, X, Y, Z, Out) :- Term = T1 / T2, eval(T1, X, Y, Z, Out1), eval(T2, X, Y, Z, Out2), {Out2 =\= 0}, {Out = Out1/Out2}.

/*
  simplify(Term, Out): simplify expression Term of numbers and atoms to simpler expressions.
*/

simplify(Atom, Out) :- atomic(Atom), Out = Atom.
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
simplify(D/F, E) :- D \= 0, F \= 0, simplify(D, E1), simplify(F, E2), ((D = E1, F = E2, E = D/F); (dif(D, E1); dif(F, E2)), simplify(E1/E2, E)).
simplify(D/F, E) :- D = LD * F, simplify(LD, E).
simplify(D/F, E) :- D = F * RD, simplify(RD, E).
simplify(D/F, E) :- D = LD * F * RD, simplify(LD*RD, E).
simplify(D/F, E) :- F = LD * D, simplify(1/LD, E).
simplify(D/F, E) :- F = D * RD, simplify(1/RD, E).
simplify(D/F, E) :- F = LD * D * RD, simplify(1/(LD*RD), E).
simplify(D/F, E) :- D = T * RD, F = T * RF, simplify(RD/RF, E).
simplify(D/F, E) :- D = LD * T, F = LF * T, simplify(LD/LF, E).
simplify(D/F, E) :- D = LD * T, F = T * RF, simplify(LD/RF, E).
simplify(D/F, E) :- D = T * RD, F = LF * T, simplify(RD/LF, E).
simplify(D/F, E) :- D = T * RD, F = LF * T * RF, simplify(RD/(LF*RF), E).
simplify(D/F, E) :- D = LD * T, F = LF * T * RF, simplify(LD/(LF*RF), E).
simplify(D/F, E) :- D = LD * T * RD, F = T * RF, simplify((LD*RD)/RF, E).
simplify(D/F, E) :- D = LD * T * RD, F = LF * T, simplify((LD*RD)/LF, E).
simplify(D/F, E) :- D = LD * T * RD, F = LF * T * RF, simplify((LD*RD)/(LF*RF), E).
simplify(A/B/C, E) :- simplify(A/(B*C), E).
simplify(-D, E) :- simplify(D, E1), E = -E1.

/*
  eval_zero(Term): check whether Term evaluates at [0, 1, 0] to zero or not.
*/

eval_zero(Term) :- simplify(Term, Out), eval(Out, 0, 1, 0, 0).

/*
  needle_haystack(N, H): check whether an atom N exists in H. (https://stackoverflow.com/a/57602983/8757529)
*/
needle_haystack(N,H) :- N==H.
needle_haystack(N,H) :- H=..[_|As],member(A,As),needle_haystack(N,A).

/*
  eval_proj(TermX, TermY, TermZ, OutX, OutY, OutZ): evaluate [TermX; TermY; TermZ] (projective coordinates) into [OutX; OutY; OutZ].
*/

eval_proj(TermX, TermY, TermZ, OutX, OutY, OutZ) :-
  TermX = Lambda * SubtermX,
  TermY = Lambda * SubtermY,
  TermZ = Lambda * SubtermZ,
  eval_proj(SubtermX, SubtermY, SubtermZ, OutX, OutY, OutZ).

eval_proj(TermX, TermY, TermZ, OutX, OutY, OutZ) :-
  eval(TermX, InterX),
  eval(TermY, InterY),
  eval(TermZ, InterZ),
  simplify(InterX, RetX),
  simplify(InterY, RetY),
  simplify(InterZ, RetZ),
  (
    (needle_haystack(oo, RetX), eval_proj(x*TermX, x*TermY, x*TermZ, OutX, OutY, OutZ));
    (needle_haystack(oo, RetY), eval_proj(y*TermX, y*TermY, y*TermZ, OutX, OutY, OutZ));
    (needle_haystack(oo, RetZ), eval_proj(z*TermX, z*TermY, z*TermZ, OutX, OutY, OutZ));
    (RetX = 0, RetY = 0, RetZ = 0, (
      eval_proj(TermX/x, TermY/x, TermZ/x, OutX, OutY, OutZ);
      eval_proj(TermX/y, TermY/y, TermZ/y, OutX, OutY, OutZ);
      eval_proj(TermX/z, TermY/z, TermZ/z, OutX, OutY, OutZ)
    ));
    (number(RetX), number(RetY), number(RetZ), OutX is RetX, OutY is RetY, OutZ is RetZ)
  ).


/*ratmap(TermX, TermY, TermZ) :-*/