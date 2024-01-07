
(*
exploration of languages
thinking of how they go wrong 
how they might be implemented

thankfully standard ml has mutable arrays

emacs set sml-mode on sml file
M-x sml-mode

C-c C-r  evaluate selected region in emacs and send to standard ml program
C-c C-b  send entire buffer to be evaluated

*)

val a = 1 ;
val b = 2;
val c = 3;

val arr = Array.fromList [ 1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 13, 1, 19, 1, 19, 10, 23, 2, 10, 23, 27, 1, 27, 6, 31, 1, 13, 31, 35, 1, 13, 35, 39, 1, 39, 10, 43, 2, 43, 13, 47, 1, 47, 9, 51, 2, 51, 13, 55, 1, 5, 55, 59, 2, 59, 9, 63, 1, 13, 63, 67, 2, 13, 67, 71, 1, 71, 5, 75, 2, 75, 13, 79, 1, 79, 6, 83, 1, 83, 5, 87, 2, 87, 6, 91, 1, 5, 91, 95, 1, 95, 13, 99, 2, 99, 6, 103, 1, 5, 103, 107, 1, 107, 9, 111, 2, 6, 111, 115, 1, 5, 115, 119, 1, 119, 2, 123, 1, 6, 123, 0, 99, 2, 14, 0, 0 ] ;

(*
arr[0] Array.sub(arr,0)
arr[1] = 12  Array.update(arr,1,12)

check we can copy array mutate and not change original
copy {src , dest , }

ignore type annotations at beginning

try writing what need for a specific example to see have correct notation
for records curly braces , key , equals , value , comma seperates key - value pairs
{ key = value , key2 = value2 ....}

Array.copy {src = arr , dst = Array.array(133,0) , di = 0 } ;

copy array of length zero , what is the type of the array if no type is given

we do not want to have to apply a function to each element of any array since
it should just be one memory allocation
bump allocator
memptr = memptr + size of array * size of array element

what happens if request array so large as to be bigger than computer hardware
memory and any and all included discs 

 *)
fun arrayCopy arr init =
    let val len = Array.length arr
    in
	if (len < 1) then
	    Array.array(0,init)
	else	    
	    let val arr2 = Array.array(len , init)
	    in
		Array.copy {src = arr , dst = arr2 , di = 0 }  ;
		arr2
	    end
    end ;

val arr2 = arrayCopy arr 0 ; 
	
(*

this however will not type check because insists we have a type for empty array
even though it is never instantiated and array cannot be changed  
Array.array(0,_);

lets now see if changing array arr2 changes arr

Array.sub (arr , 1);
Array.sub (arr2 , 1);
Array.update(arr2 , 1 , 12);
Array.sub (arr , 1);
Array.sub (arr2 , 1);

so that worked ok

check we can define a function 

fun f x = x + 2;
f 5 ;

f


let val add = 1
    val mult = 2
in add + mult
end

recursive functions just fun
multually recursive need
    and keyword , following functions drop fun keyword

standard-ml   ocaml-ml
-----------------------
Array.sub     Array.get
Array.update  Array.set
let val a =   let a = .. in ..

let val a = 3 in
    a + a
end

standard ml functions not usually taking and returning functions?
accurate??

iter -> int array -> int -> int  

iter simulates 

in standard ml confusingly ~ squiggle represents negative 1 
i do not know what - 1 means in this language
 *)
    
		   
fun iter (v : int array) (n : int) : int = 
    let val ins = Array.sub( v , n)    (* get instruction *)
	val halt = 99 
	val add = 1 
	val mult= 2
    in
	if ins = halt then  Array.sub(v ,0)
	else if ins = add then
	    let val a = Array.sub( v ,Array.sub( v, n + 1))
		val b = Array.sub( v ,Array.sub( v, n + 2))
	    in
		Array.update( v , Array.sub(v, n + 3) , a + b) ;
		iter v (n + 4)
	    end
	else if ins = mult then 
	    let val a = Array.sub( v ,Array.sub( v, n + 1))
		val b = Array.sub( v ,Array.sub( v, n + 2))
	    in                              (* add changed to multiply *)
		Array.update( v , Array.sub(v, n + 3) , a * b) ;
		iter v (n + 4)
	    end
	else ( 0 - 1 )  (* negative one  *)
    end ;

fun run v = iter v 0 ;
	
val part1 = let val arr2 = arrayCopy arr 0 in
		Array.update(arr2 ,1 ,12) ;
		Array.update(arr2,2, 2) ;
		run arr2
	    end ;

(*
val part1 = 4090701 : int

*)

val part2 =
  let val target = 19690720
      fun iter2 (noun : int) (verb : int) (res : (int * int) list ) : (int * int) list =
	  if noun > 99 then
	      iter2 0 (verb + 1) res
	  else if verb > 99 then
	      res
	  else
	      let val arr2 = arrayCopy arr 0 in
		  Array.update(arr2,1, noun) ;
		  Array.update(arr2, 2, verb) ;
		  let val tmp = run arr2 in
		      if tmp = target then        
			  iter2 (noun + 1) verb ((noun ,verb) :: res)
		      else
			  iter2 (noun + 1) verb res
		  end
	      end
  in
      iter2 0 0 []
  end ;


(*
val part2 = [(64,21)] : (int * int) list

*)



				    
