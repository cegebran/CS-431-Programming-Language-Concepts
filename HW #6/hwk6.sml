(* Brandon Cegelski *)
(* 3/9/2018 *)
(* Homework - 6 *)

datatype exp = Const of int
             | Var of string
             | Plus of exp * exp
             | Times of exp * exp
             | Pow of exp * int;

(* Question 1 *)
fun print (Const x) = Int.toString x
  | print (Var x) = x
  | print (Plus(x,y)) = "(" ^print(x)^ " + " ^print(y)^ ")"
  | print (Times(x,y)) = "(" ^print(x)^ " * " ^print(y)^ ")"
  | print (Pow(x,y)) = "(" ^print(x)^ "^" ^ Int.toString y ^ ")";
(* Testing Question 1 *)
val e = Times (Times (Var "x", Var "y"), Plus (Var "x", Const 3));
val e1 = Pow (Var "x", 4);
print e;
print e1;

(* Question 2 *)
fun deriv (Var x) a = if(x = a) then Const 1 else Const 0
  | deriv (Const x) a = Const 0
  | deriv (Pow (p,y)) a =
	let
	  val yNew = y - 1
	in
	  Times( Times(Const y, Pow(p, yNew)), Const 1)
	end
  | deriv (Times (x,y)) a =
	let
	  val xDeriv = deriv x a
	  val yDeriv = deriv y a
	in
	  Plus (Times (xDeriv, y), Times (x, yDeriv))
	end
  | deriv (Plus(x,y)) a =
	let
	  val xDeriv = deriv x a
	  val yDeriv = deriv y a
	in
	  Plus (xDeriv, yDeriv)
	end;
(* Testing Question 2 *)
print (deriv e "x");
print (deriv e1 "x");

(* Question 3 *)

(* Helper Function *)
fun simp (Times(Const 1, y)) = y
  | simp (Times(x, Const 1)) = x
  | simp (Plus(Const 0, y)) = y
  | simp (Plus(x, Const 0)) = x
  | simp (Times(Const 0, y)) = Const 0
  | simp (Times(x, Const 0)) = Const 0
  | simp (Times(x,y)) = Times(x,y)
  | simp (Plus(x, y)) = Plus(x,y)
  | simp (Pow(x,y)) = Pow(x,y)
  | simp (Const x) = Const x
  | simp (Var x) = Var x;

fun simplify (Const x) = (Const x)
  | simplify (Var x) = (Var x)
  | simplify (Plus(x,y)) = simp(Plus(simplify(x), simplify(y)))
  | simplify (Times(x,y)) = simp(Times(simplify(x), simplify(y)))
  | simplify (Pow(x,y)) = simp((Pow(simplify(x),y)));
(* Testing Question 3 *)
print (simplify (deriv e "x"));
print (simplify (deriv e1 "x"));
val e2 = Pow(Plus (Var "x", Const 0), 2);
print e2;
print (simplify e2);