
(* for this type of problem it appears standard ml is too brittle 
cannot seem to update tape correctly ?

ok - this was a programming err on my part !

*)
(* sml-mode polyml day5 cont/d from day2 *)
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
		     

(* parameter modes *)
fun parameter_opcode (n:int) : int = n mod 100

fun parameter_mode1 (n:int) : int =
    if n < 100 then 0 else (n mod 1000) div 100

fun parameter_mode2 (n:int) : int =
    if n < 1000 then 0 else (n mod 10000) div 1000
						  
fun parameter_mode3 (n:int) : int =
    if n < 10000 then 0 else (n mod 100000) div 10000
						

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


val (is_even, is_odd , santa) = 
 let fun evens n = 
	 case n of 
	     0 => true 
	   | 1 => false 
	   | _  => odds (n - 1) 
     and odds n = 
	 case n of 
	     0 => false
	   | 1  =>  true
	   | _ => evens (n-1) ;
     val fig = 25
 in (evens, odds , fig)
 end

(* shadow bindings *)

(* able to use pattern match as code compression 
 we can offload common code sections to another procedure elsewhere 

let val a = ... ; 
    val b = can use definition of a 

mutually recursive 
let rec val a = ... 
     and val b = ... ;  (a and b are a single unit)
    val c = can use a b definitions but not recursively ?huh?
*)

(*** we are here 
parameter_opcode n
parameter_mode1 2 3 

reader : 'a -> int 
we can pass anything to reader , but sml will have trouble unifying it

hint : avoid pluralised names  
 prefer one_arg two_arg three_arg 
versus plural two_args three_args 

tape 
  fun ... A.update(tape,n , 3) ...
     we find later on in the recursion that tape has not been updated ?

***)			   
fun interpret (tape : int array , reader : unit -> int , writer : int -> unit) =
    let val ip = ref 0 ;
	(* val tape = ref tp ; *)
	fun one_arg_read () = 
	    let val pcode = A.sub(tape , !ip) ;
		val opcode = parameter_opcode pcode;
		val p1 = parameter_mode1 pcode;
		val arg1 = A.sub(tape, (!ip) + 1) ; 
		val v1 = arg1 (* A.sub(tape,arg1)*) (* v1 is positional*) 
	    in (v1)
	    end ;
	fun one_arg_write () = 
	    let val pcode = A.sub(tape , !ip) ;
		val opcode = parameter_opcode pcode;
		val p1 = parameter_mode1 pcode;
		val arg1 = A.sub(tape, (!ip) + 1) ; 
		val v1 = (case p1 of 0 => A.sub(tape, arg1) | _ => arg1);	   
	    in (v1)
	    end ;
	fun two_arg () = 
	    let val pcode = A.sub(tape , !ip) ;
		val opcode = parameter_opcode pcode;
		val p1 = parameter_mode1 pcode;
		val p2 = parameter_mode2 pcode; 
                (* val p3 = parameter_mode3 pcode; *)
		val arg1 = A.sub(tape, (!ip) + 1) ; 
		val arg2 = A.sub(tape, (!ip) + 2) ;
		(* val arg3 = A.sub(tape, (!ip) + 3) ; *)
		val v1 = (case p1 of 0 => A.sub(tape, arg1) | _ => arg1);	   
		val v2 = (case p2 of 0 => A.sub(tape, arg2) | _ => arg2);	   
		(* val v3 = arg3   *)
	    in (v1,v2)
	    end ;
	fun three_arg () = 
	    let val pcode = A.sub(tape , !ip) ;
		val opcode = parameter_opcode pcode;
		val p1 = parameter_mode1 pcode;
		val p2 = parameter_mode2 pcode; 
                val p3 = parameter_mode3 pcode;
		val arg1 = A.sub(tape, (!ip) + 1) ; 
		val arg2 = A.sub(tape, (!ip) + 2) ;
		val arg3 = A.sub(tape, (!ip) + 3) ;
		val v1 = (case p1 of 0 => A.sub(tape, arg1) | _ => arg1);	   
		val v2 = (case p2 of 0 => A.sub(tape, arg2) | _ => arg2);	   
		val v3 = arg3  
	    in (v1,v2,v3)
	    end ;
	fun interpret_loop () =
	    (
	      (* ?? debugger for standard ml ?? 
      print "tape = " ; 
      A.app printNum tape ; 	(* printList Int.toString tape ;  *)
      print "\n";
	      *)

	      let val raw = (A.sub(tape , !ip)) ;
		  val opcode = parameter_opcode raw
	      in
		 
	      (*
	      if (!ip) = 2 then 
		  (print "ip_2 check = " ; print (Int.toString (!ip)) ; 
		   print "\n" ;
		  print "at 225 is  = " ; print (Int.toString (A.sub(tape,225))) ; 
		  print "\n" 
		  ) else () ;
		 
	      print "ip = " ; print (Int.toString (!ip)) ; print "\n" ;
	      print "raw code = " ; print (Int.toString raw) ; print "\n" ;
	      print "opcode = " ; print (Int.toString opcode) ; print "\n" ;
		 *)
	      case opcode of 
		  1 => interpret_add ()
		| 2 => interpret_mult ()
		| 3 => interpret_read ()
		| 4 => interpret_write ()
		| 5  => interpret_jump_if_true ()
		| 6  => interpret_jump_if_false ()
		| 7  => interpret_less_than ()
		| 8  => interpret_equals ()
		| 99 => (tape,!ip,true)
		| n  => (print "unrecognised opcode " ; 
		         print (Int.toString opcode) ; 
			 print " ? \n"; 
			 (tape,!ip,false))
	      end) 	
	and interpret_equals () = 
	    let val (v1,v2,v3) = three_arg () ;
		val res = if v1 = v2 then 1 else 0 			   
	    in 
		(A.update(tape, v3 , res) ; 
		 ip := (!ip) + 4 ; 
		 interpret_loop ())
	    end			      
	and interpret_less_than () = 
	    let val (v1,v2,v3) = three_arg () ;
		val res = if v1 < v2 then 1 else 0 			   
	    in 
		(A.update(tape, v3 , res) ; 
		 ip := (!ip) + 4 ; 
		 interpret_loop ())
	    end			      
	and interpret_jump_if_true () = 
	    let val (v1,v2) = two_arg ()
	    in if not (v1 = 0)
	       then (ip := v2 ; interpret_loop ())
	       else (ip := (!ip) + 3 ; interpret_loop ()) 
	    end
	and interpret_jump_if_false () = 
	    let val (v1,v2) = two_arg ()
	    in if v1 = 0
	       then (ip := v2 ; interpret_loop ())
	       else (ip := (!ip) + 3 ; interpret_loop ()) 
	    end		
	and interpret_read () = 
	    let val (v1) = one_arg_read () (*piggyback interpret_write *)
	    in 
		(A.update(tape, v1 , reader()) ;   (* write directly to value in one_arg *)
		 (*
		 print "check read : index " ;	print (Int.toString v1) ; print " " ;
		 print (Int.toString (A.sub(tape,v1))) ; 
		 print " <> "; print (Int.toString 1) ; 
		 print "\n" ;
		 *)
		 ip := (!ip) + 2 ; 
		 interpret_loop ())
	    end			      		      
	and interpret_write () =  
	    let val (v1) = one_arg_write ()
	    in    (* awkward write indirectly to value one_arg *)
		(writer v1 ; (*(A.sub(tape,v1)) ;*) 
		 ip := (!ip) + 2 ; 
		 interpret_loop ())
	    end			      
	and interpret_add () = 
	    let val (v1,v2,v3) = three_arg ()
	    in 
		(
		 (* print "add " ; print (Int.toString v1) ; print " + ";
		  print (Int.toString v2) ; print " => ";
		  print (Int.toString v3) ; print "\n" ;
		  *)
		 A.update(tape, v3 ,v1 + v2) ; 
		  (*print "check " ; print (Int.toString (A.sub(tape,v3))) ; 
		  print " <> "; print (Int.toString (v1 + v2)) ; 
		  print "\n" ;
                  *)
		 ip := (!ip) + 4 ; 
		 interpret_loop ())
	    end			      
	and interpret_mult () =
	    let val (v1,v2,v3) = three_arg ()
	    in 
		(A.update(tape, v3 ,v1 * v2) ;
		 ip := (!ip) + 4;
		 interpret_loop ())
	    end			      
    in 
	interpret_loop ()
    end
       

	

(* here we have a most general polymorphic function takes anything which is ignored ,
  and returns me a fixed int *)			       

(* val rec reader : type -> type = definition *)			    
val reader : 'a -> int
    = fn _ => 1

(* local variable so we can see what got wrote *)		  
val writer : int -> unit
    = fn n => print ("wrote " ^ (Int.toString n))

(*
tuples - we access them with so called projections ?
#1 (3,5)
#2 (3,5)
*)
		    
val writer2 =
    fn () => (
    let val notepad = (ref [] ) ;
	val write = fn (n:int) => notepad := n :: (!notepad) ;
        val read =  fn () => (rev (!notepad))
    in
	(write ,read)
    end)

val makeWriter = writer2 


(* here we have a mutable writer , reader 
each write we cons onto front of list , 
each read all we reverse entire list - and return it 
*)

val (writer,writer_notepad) = writer2 () 
(* val write1 = #1 w1; *)
(* val read1 = #2 w1; *)
(* val _ = (List.map (fn n => write1 n) [1,2,3,4,5,6,7,8,9,0] ; () ) *)
(* val _ = (List.map (writer) [1,2,3,4,5,6,7,8,9,0] ; () ) *)
val wrote1 = writer_notepad ()
val reader : unit -> int = fn () => 1
fun v2day2part1 () = interpret(day2tape() , reader ,writer )

(* one drawback standard ml uses ~ for negative numbers ? *)
fun day5tape () = A.fromList [3,225,1,225,6,6,1100,1,238,225,104,0,1001,152,55,224,1001,224,~68,224,4,224,1002,223,8,223,1001,224,4,224,1,224,223,223,1101,62,41,225,1101,83,71,225,102,59,147,224,101,~944,224,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,2,40,139,224,1001,224,~3905,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1101,6,94,224,101,~100,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1102,75,30,225,1102,70,44,224,101,~3080,224,224,4,224,1002,223,8,223,1001,224,4,224,1,223,224,223,1101,55,20,225,1102,55,16,225,1102,13,94,225,1102,16,55,225,1102,13,13,225,1,109,143,224,101,~88,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1002,136,57,224,101,~1140,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,101,76,35,224,1001,224,~138,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,344,101,1,223,223,1107,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,374,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,389,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,404,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,419,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,434,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,449,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,464,1001,223,1,223,8,226,226,224,1002,223,2,223,1005,224,479,1001,223,1,223,7,226,677,224,102,2,223,223,1006,224,494,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,509,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,524,101,1,223,223,1007,677,226,224,102,2,223,223,1006,224,539,101,1,223,223,107,226,226,224,1002,223,2,223,1006,224,554,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,584,101,1,223,223,1107,226,677,224,102,2,223,223,1005,224,599,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,614,101,1,223,223,108,677,226,224,102,2,223,223,1005,224,629,101,1,223,223,107,226,677,224,102,2,223,223,1006,224,644,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,108,226,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]


fun day5part1 () = 
   let val reader : unit -> int = fn _ => 1 ;  (* always returns 1 *)
       val (writer : int -> unit ,notepad : unit -> int list) = makeWriter() ;
       val out = interpret(day5tape() , reader ,writer ) 
   in 
      print "finished state \n" ; 
      (out , "wrote =" , notepad ())
   end

fun letc () = let val a = 3 	(* 3 *)
	          val b = a + a (* 3 + 3 or 6 *)
                  val c = b + b (* 6 + 6 or 12 *)
	      in c  		(* c = 12 *)
	      end

		  
(*		    
structure Writer = 
  struct
    (* Type definitions *)
    type t = ...

    (* Values *)
    val x = 10

    (* Functions *)
    fun add a b = a + b
  end;

3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not). -- done
3,9,7,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not). -- o
3,3,1108,-1,8,3,4,3,99 - Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not). -- todo
3,3,1107,-1,8,3,4,3,99 - Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not). --todo 

copy Array ? ensure not mutated ?

*)			

fun day5 (tape , n) = 
   let val reader : unit -> int = fn _ => n ;  
       val (writer : int -> unit ,notepad : unit -> int list) = makeWriter() ;
       val out = interpret(tape , reader ,writer ) 
   in 
      print "finished state \n" ; 
      (out , "wrote =" , notepad ())
   end

(* caveat negative numbers are squiggles ~1 is negative 1 ?? *)       

(*
3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
tests equal to 
*)
val test_eq1 = fn () => day5 (A.fromList [3,9,8,9,10,9,4,9,99,~1,8] , 8) (* 1 *)
val test_eq2 = fn () => day5 (A.fromList [3,9,8,9,10,9,4,9,99,~1,8] , 12) (* 0 *)

(*
3,9,7,9,10,9,4,9,99,~1,8 - Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
*)
val test_lt1 = fn () => day5 (A.fromList [3,9,7,9,10,9,4,9,99,~1,8] , 5) (* 1 yes 5 < 8*)
val test_lt2 = fn () => day5 (A.fromList [3,9,7,9,10,9,4,9,99,~1,8] , 8) (* 0 no 8 < 8 *)

(*
3,3,1108,~1,8,3,4,3,99 - Using immediate mode, consider whether the input is equal to 8;
output 1 (if it is) 
output 0 (if it is not). 
*)
val test_eq3 = fn () => day5 (A.fromList [3,3,1108,~1,8,3,4,3,99] , 8) (* expect 1 *)
val test_eq4 = fn () => day5 (A.fromList [3,3,1108,~1,8,3,4,3,99] , 12) (* expect 0 *)
			  
(*
3,3,1107,~1,8,3,4,3,99 - Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not). --todo

reads value into position 3 where we have ~1
 1107 means less than 5 < 8   11 means immediate mode both args

*)
val test_lt3 = fn () => day5 (A.fromList [3,3,1107,~1,8,3,4,3,99] , 5) (* 1 yes 5 < 8 *)
val test_lt4 = fn () => day5 (A.fromList [3,3,1107,~1,8,3,4,3,99] , 8) (* 0 no 8 < 8 *)
			 
(* 
Here are some jump tests that take an input, then output 0 if the input was zero 
or 1 if the input was non-zero:
3,12,6,12,15,1,13,14,13,4,13,99,~1,0,1,9 (using position mode)
 *)			       
val (test_jmp1,test_jmp2) = 
    let val xs = [3,12,6,12,15,1,13,14,13,4,13,99,~1,0,1,9]
    in (fn () => day5 (A.fromList xs, 0) , (* expect 0 *)
	fn () => day5 (A.fromList xs, 10)) (* expect 1 *)
    end 

(* 
Here are some jump tests that take an input, then output 0 if the input was zero 
or 1 if the input was non-zero:
3,3,1105,~1,9,1101,0,0,12,4,12,99,1 (using immediate mode)
 *)			       
val (test_jmp3,test_jmp4) = 
    let val xs = [3,3,1105,~1,9,1101,0,0,12,4,12,99,1]
    in (fn () => day5 (A.fromList xs, 0) , (* expect 0 *)
	fn () => day5 (A.fromList xs, 10)) (* expect 1 *)
    end 

(* 
3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99

The above example program uses an input instruction to ask for a single number. The program will then output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or output 1001 if the input value is greater than 8.
 *)

(* val _ = PolyML.Debugger.debug := true;			 *)
val _ = PolyML.Compiler.debug := true;

(* https://www.polyml.org/documentation/Tutorials/Debugging.html *)
open PolyML.Debug;

(* trace true *)
(* trace false *)
	       
val (test_big1,test_big2, test_big3) = 
 let val xs = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
 in 
     (fn () => day5 (A.fromList xs,7) , 	(* expect 999 if input < 8 *)
      fn () => day5 (A.fromList xs,8) , 	(* expect 1000 if input 8 *)
      fn () => day5 (A.fromList xs,9) ) 	(* expect 1001 if input > 8  *)
 end

(* how do we copium with crashing procedure like subscript out of bounds 
  https://www.polyml.org/documentation/Papers/poly/polymanual.html 
*)


(* if a language is not bothered to document how its supposed to work ,
 why bother
1982 polyml uses begin ... end , this is not used anymore
 (begin
	    10 div 0
	catch proc (name: string) integer
	 begin
	  print("Exception-");
          print(name);
	  9999
         end
        end)

actually
  10 div 0 ;
Exception- Div raised

10 div 0 handle Div => 999  ; 

 *)

fun day5part2 () = day5 (day5tape() , 5)

val day7list = [3,8,1001,8,10,8,105,1,0,0,21,46,67,76,97,118,199,280,361,442,99999,3,9,1002,9,3,9,101,4,9,9,102,3,9,9,1001,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,101,5,9,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,101,4,9,9,4,9,99,3,9,1001,9,4,9,102,2,9,9,1001,9,4,9,1002,9,5,9,4,9,99,3,9,102,3,9,9,1001,9,2,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99] 			

fun day7tape () = A.fromList day7list 

(* reload shorthand *)
fun rl () = use "fun.sml";


(* permute 0 1 2 3 4
 either as a collection or just one by one
0 1 2 3 4 
 *)


(* phase setting followed by a input from previous amplifier ,
  if no previous amplifier use value 0
  we need a compound reader that will give multiple values

  makeReader (phase , in ) 

   caveat in is a reserved word 
 *)
fun makeReader (phase :int ,input : int) =
    let val state = ref 0
    in fn () => (case !state of  (* remember we read , forever else just give input *)
		     0 => (state := (!state) + 1 ; phase)
		   | _ => input)
    end
			
val r = makeReader (10 , 3 )
		   
fun day7 (tape , phase , input ) : int = 
   let val reader = makeReader (phase,input) ;  
       val (writer ,notepad ) = makeWriter() ;
       val out = interpret(tape , reader ,writer ) 
   in 
      (* print "finished state \n" ; 
      (out , "wrote =" , notepad ())
       *)
       case notepad () of
	   [n] => n
	| xs => List.last xs
   end
       
(*
Here are some example programs:
Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0
caveat ~ for - negative numbers

remember we need a fresh tape each time 
*)
val trial1 = let fun mk_tape () =
		     A.fromList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] ;
		 val out1 = day7(mk_tape (), 4 , 0) ; (* 0 default no prev *)
		 val out2 = day7(mk_tape (), 3 , out1) ;
		 val out3 = day7(mk_tape (), 2 , out2) ;
		 val out4 = day7(mk_tape (), 1 , out3) ;
		 val out5 = day7(mk_tape (), 0 , out4)
	     in out5
	     end

(*
Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):

3,23,3,24,1002,24,10,24,1002,23,~1,23,101,5,23,23,1,24,23,23,4,23,99,0,0
*)
val trial2 = (fn (a,b,c,d,e) =>
		let fun mk_tape () =
		     A.fromList [3,23,3,24,1002,24,10,24,1002,23,~1,23,101,5,23,23,1,24,23,23,4,23,99,0,0];
		 val out1 = day7(mk_tape (), a , 0) ; (* 0 default no prev *)
		 val out2 = day7(mk_tape (), b , out1) ;
		 val out3 = day7(mk_tape (), c , out2) ;
		 val out4 = day7(mk_tape (), d , out3) ;
		 val out5 = day7(mk_tape (), e , out4)
	     in out5
	     end)(0,1,2,3,4)
		 
(*
Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):

3,31,3,32,1002,32,10,32,1001,31,~2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0

*)

val trial3 = (fn (a,b,c,d,e) =>
		let fun mk_tape () =
		     A.fromList [3,31,3,32,1002,32,10,32,1001,31,~2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0];
		 val out1 = day7(mk_tape (), a , 0) ; (* 0 default no prev *)
		 val out2 = day7(mk_tape (), b , out1) ;
		 val out3 = day7(mk_tape (), c , out2) ;
		 val out4 = day7(mk_tape (), d , out3) ;
		 val out5 = day7(mk_tape (), e , out4)
	     in out5
	     end)(1,0,4,3,2)
		 		 


(* will be 120 perms of 0 1 2 3 4 inclusive , no 5 *)

(*
painful - trying to make permutations 

fun perms () = 
    let val count = ref 1 
	fun permute (xs , acc) = 
    case xs of 
	[] => (print "permutation " ; print (Int.toString (!count)) ;
              printList (Int.toString) acc ; print "\n" ; 
	      count := (!count) + 1 ; () ) 
     | (h::t) => (permute (t, (h :: acc)) ;
		  permute (t @ [h] , acc ))
    in 
	permute ([0,1,2,3,4], [])  
    end
*)


(* lot time wasted on curried / non curried functions  *)
fun without (n : int)  (xs : int list) : int list = 
    List.filter (fn m => not (n = m)) xs


(* choice 0 1 2 3 4 *)
fun perms () = 
  let val count = ref 1 
      fun perm xs ys =  
      case xs of 
       [] => (print "permutation : " ; print (Int.toString (!count)) ; 
              print " : " ;
	      printList (Int.toString) ys ; 
	      count := (!count) + 1 )
       | _ => let 
	      in List.map (fn n => perm (without n xs) (n :: ys)) xs ; () 
	      end
  in perm [0,1,2,3,4] []
  end


(* *)    
fun day7perms (tape_list) = 
  let val count : int ref = ref 1 ;
      val highest : int ref = ref 0 ; 
      val highest_list : int list ref= ref [] ; 
      fun trial vars = 
	  case vars of 
	      [a,b,c,d,e] => 
		let fun mk_tape () =
		     A.fromList tape_list;
		 val out1 = day7(mk_tape (), a , 0) ; (* 0 default no prev *)
		 val out2 = day7(mk_tape (), b , out1) ;
		 val out3 = day7(mk_tape (), c , out2) ;
		 val out4 = day7(mk_tape (), d , out3) ;
		 val out5 = day7(mk_tape (), e , out4)
		in out5
		end  ;
      fun record (ys) = let val out = trial ys 
			in if out > (!highest) then 
			       (highest := out ;
				highest_list := ys ; ())
			   else ()
			end ;
      fun perm xs ys =  
      case xs of 
       [] => (print "permutation : " ; print (Int.toString (!count)) ; 
              print " : " ;
	      printList (Int.toString) ys ; (* ys is the goods  *)
	      record ys ;
	      count := (!count) + 1 )
       | _ => let 
	      in List.map (fn n => perm (without n xs) (n :: ys)) xs ; () 
	      end 
  in perm [0,1,2,3,4] [] ; 
     (!highest, !highest_list)
  end

val trial1b = day7perms [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
val trial2b = day7perms [3,23,3,24,1002,24,10,24,1002,23,~1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
val trial3b = day7perms [3,31,3,32,1002,32,10,32,1001,31,~2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

val day7part1 = day7perms day7list

(* 
> use "fun.sml" 
val day7part1 = (67023, [2, 3, 1, 0, 4]): int * int list

not quite clear on the semantics of
do we use a new tape each time ?

*)



