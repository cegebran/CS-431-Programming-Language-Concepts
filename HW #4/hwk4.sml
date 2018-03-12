(* Brandon Cegelski *)
(* CS431 Homework #4 *)
(* 2/23/18 *)

(* Question 1 *)
fun merge_sort f = 
    fn nil => nil
     | [a] => [a]
     | L =>
         let
           fun halve nil = (nil, nil)
            |  halve [a] = ([a], nil)
            |  halve (a :: b :: rest) =
	         let
                   val (x, y) = halve rest
                 in
                   (a :: x, b :: y)
                 end
           fun merge (nil, x) = x
            |  merge (x, nil) = x
            |  merge (a::b, x::y) =
                 if f(a, x) then a :: merge(b, x::y)
                            else x :: merge(a::b, y)
           val (x, y) = halve L
          in
	    merge(merge_sort f x, merge_sort f y)
          end;
(* Testing Question 1 *)
merge_sort (op >) [0, 5, 1, ~4, 9, 11];

(* Question 2 *)
fun selection_sort f =
    fn nil => nil
     | [a] => [a]
     | L =>
         let
           fun select ([b]) = [b]
	    |  select (a :: b) =
		   let
	             val (x :: y) = select(b)
		   in
	             if f(a, x) then a :: x :: y else x :: a :: y
		   end
	    val (lowest :: rest) = select(L)
         in
	    lowest :: (selection_sort f rest)
         end; 
(* Testing Question 2 *)
selection_sort (op >) [0, 5, 1, ~4, 9, 11];

(* Question 3 *)
fun insertion_sort f =
    fn nil => nil
     | [a] => [a]
     | L =>
         let
	   fun insert (nil, x) = [x]
	     | insert (a :: b, x) =
	         if f(x, a) then x :: a :: b else a :: insert(b, x)

	   fun sort (sofar, nil) = sofar
	     | sort (sofar, first :: rest) =
		let
		  val ( sx :: sy ) = insert(sofar, first)
		in
		  sort(sx :: sy, rest)
		end
         in
	   sort(nil, L)
         end;

(* Testing Question 3*)
insertion_sort (op >) [0, 5, 1, ~4, 9, 11];
           
           
                  
           