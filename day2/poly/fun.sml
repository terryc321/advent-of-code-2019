
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
val _ = A.update(ex1,0,123)


fun printList toStringFn lst =
  let
    fun printOne [] = ()
      | printOne [x] = (print (toStringFn x); print "\n")
      | printOne (x::xs) = (print (toStringFn x ^ ", "); printOne xs)
  in
    printOne lst
  end;


(* Usage *)
printList Int.toString [10, 20, 30];
(* Output: 10, 20, 30 *)

(* Helper to print one number with a space *)
fun printNum n = (print (Int.toString n); print " ")


fun interpret (tape : int array , ip : int) : (int array * int * bool) =
    (
      (*
      print "tape = " ; 
      A.app printNum tape ; 	(* printList Int.toString tape ;  *)
      print "\n";
      *)
    let val opcode = Array.sub(tape , ip) 
    in case opcode of 
       1 => interpret_add (tape ,ip)
       | 2 => interpret_mult (tape,ip)
       | 99 => (tape,ip,true)
       | _  => (tape,ip,false) 
    end)			      
and interpret_add (tape : int array , ip : int) =
    let val opcode = A.sub(tape , ip) ;
        val arg1 = A.sub(tape, ip + 1) ; 
        val arg2 = A.sub(tape, ip + 2) ;
        val arg3 = A.sub(tape, ip + 3) ;
	val v1 = A.sub(tape, arg1);
	val v2 = A.sub(tape, arg2);
	val v3 = arg3 
    in 
	(A.update(tape, v3 ,v1 + v2) ;
	 interpret( tape , ip + 4))
    end			      
and interpret_mult (tape : int array , ip : int) =
    let val opcode = A.sub(tape , ip) ;
        val arg1 = A.sub(tape, ip + 1) ; 
        val arg2 = A.sub(tape, ip + 2) ;
        val arg3 = A.sub(tape, ip + 3) ;
	val v1 = A.sub(tape, arg1);
	val v2 = A.sub(tape, arg2);
	val v3 = arg3 
    in 
	(A.update(tape, v3 ,v1 * v2) ;
	 interpret( tape , ip + 4))
    end			      
         

fun prog1 () = interpret(A.fromList [1,9,10,3,2,3,11,0,99,30,40,50] , 0)

(* 1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2). *)
fun prog2 () = interpret(A.fromList [1,0,0,0,99] , 0)

(* 2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6). *)
fun prog3 () = interpret(A.fromList [2,3,0,3,99] , 0)

(* 2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801). *)
fun prog4 () = interpret(A.fromList [2,4,4,5,99,0] , 0)

(* 1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99. *)
fun prog5 () = interpret(A.fromList [1,1,1,4,99,5,6,0,99] , 0)

fun day2tape () = A.fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0]

fun day2part1 () = interpret(day2tape() , 0)

fun seek (a:int) (b:int) (foo : int -> int -> unit) : unit = 
    if a > 99 then seek 0 (b+ 1) foo
    else if b > 99 then ()
    else (foo a b ; seek (a+1) b foo) 


fun foo2 (a:int) (b:int) : unit = 
		       print ("a,b=" ^ (Int.toString a) ^ " " ^ (Int.toString b) ^ "\n")

fun seek2 () = let fun foo3 (a:int) (b:int) : unit = 
		       let val tape = day2tape () ; 
			   val out = (A.update(tape,1,a) ; 
			              A.update(tape,2,b) ;
				      interpret(tape , 0))
		       in case out of 
			      (arr,ip,true) => 
			      let val a0 = A.sub(arr,0) 
			      in if a0 =  19690720 then print ("solution = " ^ (Int.toString a) ^ " " ^ 
							       (Int.toString b) ^ " " ^
		              (Int.toString (100*a + b)) ^ "\n")

				 else ()		
			      end
			   | _ => () (* not a solution *)
		       end 
	       in 
		   seek 0 0 foo3 
	       end 

fun day2part2 () = seek2 () 

(* *)
fun seek4 (a:int , b:int , foo : int -> int -> unit) : unit = 
    if a > 99 then seek4 (0 ,b+ 1, foo)
    else if b > 99 then ()
    else (foo a b ; seek4 (a+1 , b , foo))


(* Set the limit to 100 items *)
val _ = PolyML.print_depth 100 




		       
