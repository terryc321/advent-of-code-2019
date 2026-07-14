
(* sml-mode polyml day2 *)
(* use fun.sml *)

(* side quest 1 - can we have infinite precision integers for our advent code machine 
COMPLETED
*)

fun double x = x + x

(* [1,2,3] *)

(* in general we want infinite precision integers over fixnums *)

(* Import the structure if not already open *)
structure I = IntInf;
structure A = Array;


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

val rec facr2 = 
  fn (m : I.int) => (
   fn (acc : I.int) => 
      (let val one = I.fromInt 1 ;
 		       val two = I.fromInt 2
		   in if (I.< (m ,two)) then acc 
		      else (facr2 (I.-( m, one)) (I.* (acc, m)))
		   end))
val h100 = facr2 (I.fromInt 100) (I.fromInt 1)
(* val i100 = facr (I.fromInt 100) (I.fromInt 1) *)

(* fac 100 *)
(* fac 10 *)

(* SIDE QUEST - MAKE A ONE D ARRAY  *)
val ex1 = A.fromList [1,9,10,3,2,3,11,0,99,30,40,50]
(* to get nth element of array ex1 *)
val ex1_0 = A.sub(ex1,0)
(* length of array *)
val len = A.length(ex1)
(* set nth element of array *)
val _ = Array.update(ex1,0,123)


fun interpret (tape : int array , ip : int) =
    let val opcode = Array.sub(tape , ip) 
    in case opcode of 
       0 => interpret_add (tape ,ip)
       | 1 => interpret_mul (tape,ip)
    end			      
	
fun interpret_add (tape : int array , ip : int) =
    let val opcode = Array.sub(tape , ip) ;
        val arg1 = Array.sub(tape, ip + 1) ; 
        val arg2 = Array.sub(tape, ip + 2) ;
        val arg3 = Array.sub(tape, ip + 3) 
    in 
    end			      

		    
         



		       
