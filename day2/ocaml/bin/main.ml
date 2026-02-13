let () = print_endline "Hello, World!"

(*
let input = [| 1; 0; 0; 3; 1; 1; 2; 3; 1; 3; 4; 3; 1; 5; 0; 3; 2; 13; 1; 19; 1; 19; 10; 23; 2; 10; 23; 27; 1; 27; 6; 31; 1; 13; 31; 35; 1; 13; 35; 39; 1; 39; 10; 43; 2; 43; 13; 47; 1; 47; 9; 51; 2; 51; 13; 55; 1; 5; 55; 59; 2; 59; 9; 63; 1; 13; 63; 67; 2; 13; 67; 71; 1; 71; 5; 75; 2; 75; 13; 79; 1; 79; 6; 83; 1; 83; 5; 87; 2; 87; 6; 91; 1; 5; 91; 95; 1; 95; 13; 99; 2; 99; 6; 103; 1; 5; 103; 107; 1; 107; 9; 111; 2; 6; 111; 115; 1; 5; 115; 119; 1; 119; 2; 123; 1; 6; 123; 0; 99; 2; 14; 0; 0 |]
 *)

let input =  [  1; 0; 0; 3; 1; 1; 2; 3; 1; 3; 4; 3; 1; 5; 0; 3; 2; 13; 1; 19; 1; 19; 10; 23; 2; 10; 23; 27; 1; 27; 6; 31; 1; 13; 31; 35; 1; 13; 35; 39; 1; 39; 10; 43; 2; 43; 13; 47; 1; 47; 9; 51; 2; 51; 13; 55; 1; 5; 55; 59; 2; 59; 9; 63; 1; 13; 63; 67; 2; 13; 67; 71; 1; 71; 5; 75; 2; 75; 13; 79; 1; 79; 6; 83; 1; 83; 5; 87; 2; 87; 6; 91; 1; 5; 91; 95; 1; 95; 13; 99; 2; 99; 6; 103; 1; 5; 103; 107; 1; 107; 9; 111; 2; 6; 111; 115; 1; 5; 115; 119; 1; 119; 2; 123; 1; 6; 123; 0; 99; 2; 14; 0; 0 ]


(*
  opcode 0 = add
  opcode 1 = multiply
  opcode 99 = halt
  opcode _ = something went wrong
  0 p1 p2 p3 == add values at positions p1 p2 store into p3

  tape -> tape
  1d array 
  step t:tape p:pos
 *)

type position = Pos of int
type status =  Ok |
               Bad |
               Halt
type length = Len of int
type tape = Tape of int array * status * position * length


(* do we really need to explicitly pull length of array out into another parameter though ,
   efficiency wise will it contribute anything reeally?? *)

let mk_tape (i: int list) : tape = Tape (Array.of_list i,Ok,Pos 0, Len (List.length i))

let first_tape = mk_tape input

let ex1_tape = mk_tape [1; 0; 0; 0; 99]
let ex2_tape = mk_tape [2; 3; 0; 3; 99]
let ex3_tape = mk_tape [2; 4; 4; 5; 99; 0]
let ex4_tape = mk_tape [1; 1; 1; 4; 99; 5; 6; 0; 99]



(* make a good tape   Tape(input,Ok, Pos 0)
   make a bad tape    Tape(input,Bad , Pos 0)
 *)

(* copy array prevent spurious error creep - return a tuple *)

let mytest () = let oldarr = [| 1; 2; 3 |] in
                let arr = Array.copy oldarr in                
                Array.set arr 0 999 ;
                (arr , oldarr) 


let execute_addi (t:tape) : tape =
  match t with
  | Tape(i,Ok,Pos p,Len len) -> let ci = Array.copy i in 
                                let index1 = Array.get ci (p + 1) in
                                let index2 = Array.get ci (p + 2) in
                                let index3 = Array.get ci (p + 3) in
                                let val1 = Array.get ci index1 in
                                let val2 = Array.get ci index2 in                                
                                let sum = val1 + val2 in 
                                ( Array.set ci index3 sum ;
                                  (* Printf.printf "executed ADD instruction %d + %d => %d into %d\n" val1 val2 sum index3 ; *)
                                  Tape(ci,Ok, Pos (p+4),Len len))
  | _ -> t


let execute_muli (t:tape) : tape =
  match t with
  | Tape(i,Ok,Pos p,Len len) -> let ci = Array.copy i in 
                                let index1 = Array.get ci (p + 1) in
                                let index2 = Array.get ci (p + 2) in
                                let index3 = Array.get ci (p + 3) in
                                let val1 = Array.get ci index1 in
                                let val2 = Array.get ci index2 in                                
                                let prod = val1 * val2 in 
                                ( Array.set ci index3 prod ;
                                  (* Printf.printf "executed MUL instruction %d * %d => %d into %d\n" val1 val2 prod index3 ; *)
                                  Tape(ci,Ok, Pos (p+4),Len len))
  | _ -> t



let execute_halt (t:tape) : tape =
  match t with
  | Tape(i,Ok,Pos p,Len len) -> let ci = Array.copy i in 
                                ( (* Printf.printf "executed HALT instruction \n" ; *)
                                  Tape(ci,Halt, Pos (p+4),Len len))
  | _ -> t


let rec step (t:tape) : tape =
  match t with
  | Tape(i,Ok,Pos n,Len len) -> if n >= 0 && n < len then
                                  (
                                  let opcode = Array.get i n in
                                  match opcode with
                                  | 1 -> step (execute_addi t)
                                  | 2 -> step (execute_muli t)
                                  | 99 -> execute_halt t
                                  | _ -> Tape (i,Bad,Pos n, Len len))
                                else Tape (i,Bad,Pos n, Len len)
  | Tape(i,Bad,Pos n,Len len) -> Tape(i,Bad,Pos n,Len len)

     
(* what is it that we want to find out though ? *)
(* lets simulate the tape - simply recursively step each instruction as given in puzzle *)
let sim (t:tape) : tape = step t

(* first_tape *)
let part1 () =
  match first_tape with
  | Tape(i,Ok,p,len) -> let ci = Array.copy i in 
                        ( Printf.printf "replacing position 1 with 12 \n" ;
                          Array.set ci 1 12;
                          Printf.printf "replacing position 2 with 2 \n" ;
                          Array.set ci 2 2;
                          let t = Tape(ci,Ok,p,len) in
                          sim t)



(* determine what pair of inputs produces the output 19690720
   noun at address 1
   verb at address 2
   noun and verb range from 1 to 99 inclusive

   unit == black hole

   make an initial tape with noun and verb set 
   
 *)
let mk_noun_verb_tape (t:tape) (n:int) (v:int) : tape = 
  match t with
  | Tape(i,Ok,p,len) -> let ci = Array.copy i in 
                        ( (* Printf.printf "replacing position 1 with %d \n" n ; *)
                          Array.set ci 1 n;
                          (* Printf.printf "replacing position 2 with %d \n" v; *)
                          Array.set ci 2 v;
                          let t = Tape(ci,Ok,p,len) in
                          t)



let rec recur (n:int) (v:int) : unit = if n <= 99 && v <= 99 then (
                                         (* Printf.printf "noun = %d .. verb = %d \n" n v; *)
                                         let nvt = mk_noun_verb_tape first_tape n v in
                                         let out = sim nvt in
                                         match out with
                                         | Tape(i,Halt,p,len) -> let v0 = Array.get i 0 in
                                                                 (if v0 = 19690720 then
                                                                   let nv = (100 * n) + v in
                                                                   Printf.printf "solution found .. noun = %d .. verb = %d ... 100*noun+verb = %d\n" n v nv
                                                                 else ());
                                                                  recur (n + 1) v)
                                         else if n > 99 then recur 1 (v+1)
                                       else if v > 99 then ()
                                       and brute () = recur 1 1


let part2 () = brute ()


(* *)


