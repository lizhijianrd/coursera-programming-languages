(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(w, l) =
    let
        fun exists(w, []) = false
          | exists(w, v::l) = if w = v then true else exists(w, l)
        fun remove(w, []) = []
          | remove(w, v::l) = if w = v then l else v::remove(w, l)
    in
        if exists(w, l) then SOME (remove(w, l)) else NONE
    end

fun get_substitutions1([], s) = []
  | get_substitutions1(l::ls, s) =
        case all_except_option(s, l) of 
            NONE => get_substitutions1(ls, s)
          | SOME t => t @ get_substitutions1(ls, s)

fun get_substitutions2(ll, s) =
    let
        fun f([], acc) = acc
          | f(l::ll, acc) = 
                case all_except_option(s, l) of 
                    NONE => f(ll, acc)
                  | SOME t => f(ll, acc @ t)
    in
        f(ll, [])
    end

fun similar_names(substitutions_list, full_name) =
    let
        fun f([], full_name) = []
          | f(substitution::substitution_list, full_name) = 
                case full_name of {first=first, middle=middle, last=last} =>
                    {first=substitution, middle=middle, last=last} :: f(substitution_list, full_name)
    in
        case full_name of {first=first, middle=middle, last=last} => 
            full_name :: f(get_substitutions1(substitutions_list, first), full_name)
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color c =
    case c of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value c =
    case c of
        (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
      | (_, Num x) => x

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | v::vs => if v = c then vs else v::remove_card(vs, c, e)

fun all_same_color cs =
    case cs of
        [] => true
      | _::[] => true
      | a::b::cs' => 
            card_color(a) = card_color(b) andalso all_same_color(b::cs')

fun sum_cards cs =
    let 
        fun f([], acc) = acc
          | f(c::cs, acc) = f(cs, card_value(c) + acc)
    in
        f(cs, 0)
    end

fun score(cs, goal) =
    let
        val s = sum_cards(cs)
        val ps = if s < goal then goal - s else 3 * (s - goal)
    in
        if all_same_color cs then ps div 2 else ps
    end

fun officiate(cards, moves, goal) =
    let
        fun run(_, _, [], sc) = sc
          | run([], _, Draw::_, sc) = sc
          | run(card::cards, held_cards, Draw::moves, _) =
                if sum_cards(card::held_cards) > goal 
                then score(card::held_cards, goal) 
                else run(cards, card::held_cards, moves, score(card::held_cards, goal))
          | run(_, held_cards, (Discard card)::moves, _) =
                case all_except_option(card, held_cards) of
                    NONE => raise IllegalMove
                  | SOME new_held_cards => run(cards, new_held_cards, moves, score(new_held_cards, goal))
    in
        run(cards, [], moves, score([], goal))
    end