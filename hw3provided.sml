(* Dan Grossman, CSE341 Winter 2013, HW3 Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* Problem 1~6 begin, simple string processing *)
fun only_capitals xs = 
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs = 
    foldl (fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) "" xs

fun longest_string2 xs = 
    foldl (fn (s1, s2) => if String.size s2 > String.size s1 then s2 else s1) "" xs

fun longest_string_helper f xs = 
    f xs

val longest_string3 = longest_string_helper longest_string1

val longest_string4 = longest_string_helper longest_string2

val longest_capitalized = longest_string3 o only_capitals

infix !>
fun x !> f = f x

fun rev_string s = s !> String.explode !> rev !> String.implode
(* Problem 1~6 end *)

(* Problem 7~12 begin *)
fun first_answer f xs = 
    case xs of
        [] => raise NoAnswer
       | x::xs' => case f x of
                        NONE => first_answer f xs'
                      | SOME v => v
(* Problem 7~12 end *)


