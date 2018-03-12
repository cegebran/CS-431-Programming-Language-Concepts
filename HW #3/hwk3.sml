(* Brandon Cegelski *)
(* Homework 3 *)

(* Question 1 *)
fun mean (first :: rest) = 
	let
	    fun sum (first :: rest) =
		let
		    val sumList = real (first) + sum rest
		in
		    sumList
		end
	      | sum nil = 0.0

	    fun ct (first :: rest) =
		let
		    val count = 1.0 + ct rest
		in
		    count
		end
	      | ct nil = 0.0

	in
	    sum (first :: rest) / ct (first :: rest)
	end
  | mean nil = 0.0;
(* Testing Question 2 *)
mean [1, 3, 4, 7, 9, 11, 13];

(* Question 2 *)
fun std (first :: rest) =
	let
	    fun ct (first :: rest)=
		let
		    val count = 1.0 + ct rest
		in
		    count
		end
	      | ct nil = 0.0

	    fun sum (first :: rest) =
		let
		    val sumList = real (first) + sum rest
		in
		    sumList
		end
	      | sum nil = 0.0

	    fun mean (sum, count) =
		let
		    val meanResult = sum / count
		in
		    meanResult
		end

	    fun numeratorCalc (first :: rest, mean) =
		let
		    val numCurrent = ((real(first) - mean) * (real(first) - mean)) + numeratorCalc(rest, mean)
		in
		    numCurrent
		end
	      | numeratorCalc (nil, mean) = 0.0

	in
	    Math.sqrt(numeratorCalc (first :: rest, mean(sum (first :: rest), ct (first :: rest))) / ct (first :: rest))
	end
  | std nil = 0.0;
(* Testing Question 2 *)
std [1, 3, 4, 7, 9, 11, 13];

(* Question 3 *)
fun median ((first : int) :: rest) =
	let
	    fun count (first :: rest) =
		let
		    val total = 1 + count rest
		in
		    total
		end
	      | count nil = 0

	    fun calcMedianOdd (element, first :: rest, ct) =
		let
		    val returned = first
		in
		     if (ct = element) then returned else calcMedianOdd (element, rest, ct + 1)
		end
	      | calcMedianOdd (element, nil, ct) = 0

	    fun calcMedianEven (element, first :: next :: rest, ct) =
		let
		    val returnedFirst = first
		    val returnedNext = next
		in
		    if(ct = (element - 1)) then 
			(returnedFirst + returnedNext) div 2 
		    else calcMedianEven (element, next :: rest, ct + 1)
		end
	      | calcMedianEven (element, nil, ct) = 0
	
	in
	    if ((count (first :: rest) mod 2) = 0) then calcMedianEven (((count (first :: rest)) div 2), (first :: rest), 0) else calcMedianOdd ((count (first :: rest) div 2), (first :: rest), 0)
	end

  | median nil = 0;
(* Testing Question 3 *)
median [1, 3, 4, 7, 9, 11, 13];
median [1, 3, 4, 7, 9, 11];

(* Question 4 *)
fun norm (first :: rest) =
	let
	    fun count (first :: rest) =
		let
		    val ct = 1.0 + count rest
		in
		    ct
		end
	      | count nil = 0.0

	    fun sum (first :: rest) =
		let
		    val total = real (first) + sum rest
		in
		    total
		end
	      | sum nil = 0.0

	    fun buildList ((first :: rest), mean, std) =
		let
		    val result = ((real (first) - mean) / std) :: buildList (rest, mean, std)
		in
		    result
		end
	     | buildList (nil, mean, std) = nil

	    fun numeratorCalc (first :: rest, mean) = 
		let
		    val numCurrent = ((real(first) - mean) * (real(first) - mean)) + numeratorCalc(rest, mean)
		in
		    numCurrent
		end
	      | numeratorCalc (nil, mean) = 0.0

	    fun mean (sum, count) =
		let
		    val meanResult = sum / count
		in
		    meanResult
		end
	in
	    buildList (first :: rest, (sum (first :: rest) / count (first :: rest)), Math.sqrt(numeratorCalc (first :: rest, mean(sum (first :: rest), count (first :: rest))) / count (first :: rest)))
	end
  | norm nil = 0.0 :: nil;

(* Testing Questino 4 *)
norm [1, 3, 4, 7, 9, 11, 13];























