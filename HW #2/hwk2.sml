(*Question 1 *)
fun sumAdder (arrayParam, totalSum) = if null arrayParam then totalSum else sumAdder(tl arrayParam, (totalSum + (hd arrayParam)));
fun sum sumArrayParam = if null sumArrayParam then 0 else sumAdder (sumArrayParam, 0);
(* Testing Q1 *)
sum [1, 3, 4, 7, 9, 11, 13];

(* Question 2 *)
fun meanHelperFunc (arrayParam, totalSum, totalElementCount) = if null arrayParam then (totalSum div totalElementCount) else meanHelperFunc(tl arrayParam, ((hd arrayParam) + totalSum), (totalElementCount + 1));
fun mean meanArrayParam = if null meanArrayParam then 0 else meanHelperFunc (meanArrayParam, 0, 0);
(* Testing Q2 *)
mean [1, 3, 4, 7, 9, 11, 13];

(* Question 3 *)
fun varianceHelperFunc (arrayParam, meanValue, totalVar) = if null arrayParam then totalVar else varianceHelperFunc(tl arrayParam, meanValue, (totalVar + (((hd arrayParam) - meanValue) * ((hd arrayParam) - meanValue))));
fun var varArrayParam = if null varArrayParam then 0 else varianceHelperFunc(varArrayParam, mean(varArrayParam), 0);
(* Testing Q3 *)
var [1, 3, 4, 7, 9, 11, 13];

(* Question 4 *)
fun medTotalElements (arrayParam, totalElements) = if null arrayParam then totalElements else medTotalElements((tl arrayParam), (totalElements + 1));
fun medListIndexValue (medianArrayParam, listSpot, counter) = if (counter = listSpot) then (hd medianArrayParam) else medListIndexValue((tl medianArrayParam), listSpot, (counter + 1));
fun medEvenOdd (medianArrayParam, totalElements) = if ((totalElements mod 2) = 0) then (real(medListIndexValue(medianArrayParam, ((totalElements div 2) - 1), 0)) + real(medListIndexValue(medianArrayParam, (totalElements div 2), 0))) / 2.0 else real(medListIndexValue(medianArrayParam, (totalElements div 2), 0));
fun median medianArrayParam = if null medianArrayParam then 0.0 else medEvenOdd(medianArrayParam, medTotalElements(medianArrayParam, 0));
(* Testing Q4 *)
median [1, 3, 4, 7, 9, 11, 13];