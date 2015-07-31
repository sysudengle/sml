(* Dan Grossman, CSE341 Winter 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s : string, ss : string list) = 
    let fun helper(ssh : string list, sst : string list) = 
            case sst of
                [] => NONE
               | sh2::sst2 =>   if same_string (s, sh2)
                                then SOME (ssh @ sst2)
                                else helper(ssh @ [sh2], sst2)
    in
        helper([], ss)
    end

fun get_substitutions1(sub : string list list, s : string) = 
    case sub of
         [] => []
       | h::t =>    case all_except_option(s, h) of
                         NONE => get_substitutions1(t, s)
                       | SOME xs => xs @ get_substitutions1(t, s)

fun get_substitutions2(sub : string list list, s : string) = 
    let fun aux(acc, sll) = 
        case sll of
             [] => acc
           | h::t =>    case all_except_option(s, h) of
                             NONE => aux(acc, t)
                           | SOME xs => aux(acc @ xs, t)
    in
        aux([], sub)
    end

fun similar_names(sub, name) = 
    let val {first = fst, middle = mid, last = lst} = name
        fun aux(acc, sll) = 
            case sll of
                [] => acc
              | h::t => aux(acc @ [{first = h, last = lst, middle = mid}], t)
    in
        aux([name], get_substitutions2(sub, fst))
    end
     
(*    case ss of
         [] => NONE
       | hss::tss => if same_string (s, hss)
                  then 
                  else all_except_option(s, tss)*)

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color
