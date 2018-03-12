(* Brandon Cegelski *)
(* 3-2-2018 *)
(* CS 431 - Homework 5 *)

(* Question 1 *)
fun zip (first :: firstRest, second :: secondRest) =
    let
      val (a, b) = (first, second)
    in
      if(firstRest = nil) then (a,b) :: nil else if (secondRest = nil) then (a,b) :: nil else (a,b) :: zip(firstRest, secondRest)
    end;
(* Testing Question 1 *)
zip([1, 2, 3], [true, false, true]);
zip([1, 2, 3, 4], [true, false, true]);

fun reduce f (a::b) = foldl f a b;

(* Question 2 *)
fun vectorAdd ([], []) = []
  | vectorAdd (first :: firstRest, []) = first :: firstRest
  | vectorAdd ([], second :: secondRest) = second :: secondRest
  | vectorAdd (first :: firstRest, second :: secondRest) =
      let
        val x = zip(first :: firstRest, second :: secondRest)
      in
        map (fn (a,b) => a + b) x
      end;
(* Testing Question 2 *)
vectorAdd([1,2,3], [4,5,6]);

(* Question 3 *)
fun svProduct (_, []) = []
  | svProduct (multiplier, list) =
      map (fn x => x * multiplier) list;
(* Testing Question 3 *)
svProduct(2, [1,2,3]);

(* Question 4 *)
fun vmProduct ([],[]) = []
  | vmProduct ([], matrix) = []
  | vmProduct (rowVector, []) = []
  | vmProduct (rowVector, matrix) =
      let
        fun rowVectorMult([], []) = []
          | rowVectorMult(mfirst :: mrest, matfirst :: matrest) =
              let
                fun mult(value, list) =
                  svProduct(value, list)
                val x = mult(mfirst, matfirst)
              in
                x :: rowVectorMult(mrest, matrest)
              end

        fun vectorAddFunc ([]) = []
          | vectorAddFunc (first :: nil) = first
          | vectorAddFunc (first :: second :: rest) =      
              let
                val x = vectorAdd(first, second)
              in
                vectorAddFunc(x :: rest)
              end

      in
        vectorAddFunc(rowVectorMult(rowVector, matrix))
      end;
(* Testing Question 4 *)
vmProduct([1,2,3], [[1,1],[2,1],[3,1]]);

(* Question 5 *)
fun matrixProduct ([],[]) = []
  | matrixProduct ([], matrix2) = []
  | matrixProduct (matrix1F :: matrix1Rest, matrix2) =
      let
        fun matrixMult([],[]) = []
          | matrixMult([], seconMatrix) = []
          | matrixMult(firstMatrix, secondMatrix) = 
              vmProduct(firstMatrix, secondMatrix)

        val x = matrixMult(matrix1F, matrix2)
      in
        x :: matrixProduct(matrix1Rest, matrix2)
      end;
(* Testing Question 5 *)
matrixProduct([[1,2,3],[1,1,1]],[[1,1],[2,1],[3,1]]);