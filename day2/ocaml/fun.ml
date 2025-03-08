

(* double star converts to documentation 
let x = 5 ;;

;; fore-go pain of io for now
 *)

(*
  note this is not legal ocaml , because IN is a defined word !
  let in = 3
 *)
let arr = [| 1; 0; 0; 3; 1; 1; 2; 3; 1; 3; 4; 3; 1; 5; 0; 3; 2; 13; 1; 19; 1; 19; 10; 23; 2; 10; 23; 27; 1; 27; 6; 31; 1; 13; 31; 35; 1; 13; 35; 39; 1; 39; 10; 43; 2; 43; 13; 47; 1; 47; 9; 51; 2; 51; 13; 55; 1; 5; 55; 59; 2; 59; 9; 63; 1; 13; 63; 67; 2; 13; 67; 71; 1; 71; 5; 75; 2; 75; 13; 79; 1; 79; 6; 83; 1; 83; 5; 87; 2; 87; 6; 91; 1; 5; 91; 95; 1; 95; 13; 99; 2; 99; 6; 103; 1; 5; 103; 107; 1; 107; 9; 111; 2; 6; 111; 115; 1; 5; 115; 119; 1; 119; 2; 123; 1; 6; 123; 0; 99; 2; 14; 0; 0 |] ;;

(* here is how to copy an array in ocaml *)
let b = Array.copy arr;;

Array.set b 0 999;;

b ;;

let f x = x + 2 ;;

(* cannot put stuff together

f 20 ;; f 30 ;; f 40 ;;
- : Base.Int.t = 22
!! BUG :: computed first f , rest ignored !

 *)


(* cannot use  *)
(* unused variables not allowed  *)
let run v =
  let halt = 99 in
  let add = 1 in
  let mult = 2 in
  let rec iter n =
    let ins = Array.get v n in
    if ins = halt then
      Array.get v 0
    else if ins = add then
        let a = Array.get v (Array.get v (n + 1)) in
        let b = Array.get v (Array.get v (n + 2)) in
        Array.set v (Array.get v (n + 3)) (a + b) ;
        iter (n + 4)
    else if ins = mult then 
        let a = Array.get v (Array.get v (n + 1)) in
        let b = Array.get v (Array.get v (n + 2)) in
        Array.set v (Array.get v (n + 3)) (a * b) ;
        iter (n + 4)
    else -1
  in
  iter 0 ;;


let part1 () =
  let arr2 = Array.copy arr in
  Array.set arr2 1 12 ;
  Array.set arr2 2 2 ;
  run arr2 ;;

part1 ();;


(* needed to explicitly tell ocaml the type of iter
    tuple 
 *)
let part2 () =
  let target = 19690720 in
  let rec iter noun verb res : (int * int) list =
    if noun > 99 then
      iter 0 (verb + 1) res
    else if verb > 99 then
      res
    else
      let arr2 = Array.copy arr in
      Array.set arr2 1 noun ;
      Array.set arr2 2 verb ;
      let tmp = run arr2 in
      if tmp = target then        
        iter (noun + 1) verb ((noun ,verb) :: res)
      else
        iter (noun + 1) verb res
  in
  iter 0 0 [] ;;

  
part2();;


(** 
val f : int -> int = <fun>
val run : int array -> int = <fun>
val part1 : unit -> int = <fun>
- : int = 4090701
val part2 : unit -> (int * int) list = <fun>
- : (int * int) list = [(64, 21)]
# 64 * 21;;
- : int = 1344
# 64 * 100 + 21
;;
- : int = 6421
# 
**)


   
