
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


let () = print_endline "Hello, World!"
let () = print_endline "This is the second line"
(* sequencing using ( ; )  *)
let () = (print_endline "This is the third line" ; 
         print_endline "This is the fourth line")

