
(* poly sml
8 digit  -> as a list ?
just start with a list then ..

 *)

val e = [1,5,2,4,3] ;

val e2   = [9,8,7,6,5];
val pat2 = [1,2,3]; 	(* repeat 1 2 3 *)

(* for a given input and pattern p *)
fun mulr (is : int list ,ps : int list) : int list=
    let fun recur tri =
	    case tri of
		([],_) => []
	      | (a,[]) => recur (a,ps) (* repeat rejuvinate pattern *)
	      | (ih :: it, ph :: pt) => let val rh = (ih * ph) mod 10
					in rh :: (recur (it,pt))
					end
    in recur (is,ps)	
    end

val res2 = mulr(e2,pat2);

		
				     
				     
			   

(* val r2   =  *)
