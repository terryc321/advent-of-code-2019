
(* sml-mode polyml day2 *)
(* use fun.ml *)
fun double x = x + x

(* [1,2,3] *)

(* Import the structure if not already open *)
structure I = IntInf;

val ex1 = [1,9,10,3,2,3,11,0,99,30,40,50]

(* val facr : (I.int * I.int) -> I.int *)
fun facr (m : I.int , acc : I.int) : I.int = 
	  (let val one = I.fromInt 1 ;
 	       val two = I.fromInt 2
	  in if m < two then acc 
	     else facr (I.-( m, one) , I.* (acc, m))
	  end)


fun fac (n : int) : I.int = facr (I.fromInt n , I.fromInt 1)

val f100 = fac 100

val g100 = facr (I.fromInt 100 , I.fromInt 1)



(* fac 100 *)
(* fac 10 *)


         


		       
