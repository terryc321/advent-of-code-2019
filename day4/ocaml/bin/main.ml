
(*
--- Day 4: Secure Container ---

You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:

    It is a six-digit number.
    The value is within the range given in your puzzle input.
    Two adjacent digits are the same (like 22 in 122345).
    Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

Other than the range rule, the following are true:

    111111 meets these criteria (double 11, never decreases).
    223450 does not meet these criteria (decreasing pair of digits 50).
    123789 does not meet these criteria (no double).

    How many different passwords within the range given in your puzzle input meet these criteria?

***
six digit number a b c d e f 
two adjacent digits must be the same (OR (= a b)(= b c)(= c d)(= d e)(= e f))
going left to right the digits never decrease - either same or next value is greater than previous
(AND (<= a b)(<= b c)(<= c d)(<= d e)(<= e f))

specific puzzle requirements 
in range 146810 to 612564

146810 <= val <= 612564

where val = (a * 100000) +  (b * 10000)  + (c * 1000) + (d * 100) + (e * 10) + f

values a - f can be 0 to 9 inclusive

upto program to determine which

abcdef
654321

2 :: [1;2];;

can we have a structure a b c d e f  ??

 *)

let () = print_endline "Hello, World!"
let () = print_endline "This is the second line"
(* sequencing using ( ; )  *)
let () = (print_endline "This is the third line" ; 
         print_endline "This is the fourth line")




(* lower and upper bounds allowed  *)
let lv : int = 146810
let uv : int = 612564

(* convert a to f into an int - where a is most significant digit & f is least significant *)
let cnv (a:int) (b:int) (c:int) (d:int) (e:int) (f:int) : int = (a * 100000) +  (b * 10000)  + (c * 1000) + (d * 100) + (e * 10) + f

let in_range (n : int) : bool = (n >= lv) && (n <= uv)
(* tests
   in_range lv = true
   in_range uv = true
   in_range (lv - 1 ) = false
   in_range (uv + 1 ) = false
   in_range (lv + 1) = true
   in_range (uv - 1) = true
 *)

let rec gen (i : int) (f : int -> unit ) : unit =
  if i < 10 then (f i ; gen (i + 1) f )
  else ()

(*
gen 1 (fun n -> print_endline "hi");;
 *)

(* are all values in number ascending? *)
let all_asc (a:int) (b:int) (c:int) (d:int) (e:int) (f:int) : bool =
  (a <= b) && (b <= c) && (c <= d) && (d <= e) && (e <= f)

(* adjacency *)
let has_adj (a:int) (b:int) (c:int) (d:int) (e:int) (f:int) : bool =
  (a = b) || (b = c) || (c = d) || (d = e) || (e = f)

(* adjacency2 *)
let has_adj2 (a:int) (b:int) (c:int) (d:int) (e:int) (f:int) : bool =
  ((a = b) && (b != c)) ||                           (* ABcdef *)
    ((b = c) && (not ((a = b) || (c = d)))) ||       (* aBCdef *)
      ((c = d) && (not ((b = c) || (d = e)))) ||     (* abCDef *)
        ((d = e) && (not ((c = d) || (e = f)))) ||   (* abcDEf *)
          ((e = f) && (not ((d = e))))               (* abcdEF *)
          

(* is the value generated okay - meaning passes both requirements in_range and all_ascending *)
let ok_val (a:int) (b:int) (c:int) (d:int) (e:int) (f:int) : bool =
  (all_asc a b c d e f) && (in_range (cnv a b c d e f)) && (has_adj a b c d e f)



(* can convert to string or use modulo arithmetic extract values *)
let ext_f (n : int) : int = n mod 10
let ext_e (n : int) : int = n mod 10

let ic (c : char) : int =
  match c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | ' ' -> 0

(* let ext_str (n : int) (f : string ->  *)
let ext_str (n:int) : string = Printf.sprintf "%06d" n

let simp_a (s:string) : int = ic (String.get s 0)
let simp_b (s:string) : int = ic (String.get s 1)
let simp_c (s:string) : int = ic (String.get s 2)
let simp_d (s:string) : int = ic (String.get s 3)
let simp_e (s:string) : int = ic (String.get s 4)
let simp_f (s:string) : int = ic (String.get s 5)


let all_ascending (n:int) : bool =
  let s = ext_str n in
  let a = ic (String.get s 0) in
  let b = ic (String.get s 1) in
  let c = ic (String.get s 2) in
  let d = ic (String.get s 3) in
  let e = ic (String.get s 4) in
  let f = ic (String.get s 5) in
  all_asc a b c d e f

let all_okay (n:int) : bool =
  let s = ext_str n in
  let a = ic (String.get s 0) in
  let b = ic (String.get s 1) in
  let c = ic (String.get s 2) in
  let d = ic (String.get s 3) in
  let e = ic (String.get s 4) in
  let f = ic (String.get s 5) in
  (all_asc a b c d e f) && (in_range (cnv a b c d e f)) && (has_adj a b c d e f)
  

let all_okay2 (n:int) : bool =
  let s = ext_str n in
  let a = ic (String.get s 0) in
  let b = ic (String.get s 1) in
  let c = ic (String.get s 2) in
  let d = ic (String.get s 3) in
  let e = ic (String.get s 4) in
  let f = ic (String.get s 5) in
  (all_asc a b c d e f) && (in_range (cnv a b c d e f)) && (has_adj2 a b c d e f)



(*
  simp_a (ext_str 654321);;
 *)


(* is n acceptable - if not increment next value - and try that keep going until not in_range
   keep a running total of all those acceptable
 *)

let rec counter = ref 0
and run = fun _ -> (counter := 0 ; foo lv)
and foo (n:int) : int = if n <= uv then
                          if all_okay n then
                            (counter := (!counter + 1);
                             foo (n + 1))
                          else foo (n + 1)
                        else !counter


(* we can compute part 1 okay -- part_1 ();;  *)
let part_1 () = run ()

(* can we define run2 like this ? *)
let rec counter2 = ref 0
and run2 = fun _ -> (counter2 := 0 ; foo2 lv)
and foo2 (n:int) : int = if n <= uv then
                           if all_okay2 n then
                             (counter2 := (!counter2 + 1);
                              foo2 (n + 1))
                           else foo2 (n + 1)
                         else !counter2

let part_2 () = run2 ()



(*
  #use "main.ml";;

  part_1();;
  - : int = 1748
  # part_2();;
  - : int = 1180
  # 
 *)
