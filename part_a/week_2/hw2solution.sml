(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (target, words) = 
    case words of 
        [] => NONE
      | word :: words' => if same_string(target, word) 
                          then SOME words'
                          else case all_except_option(target, words') of 
                                   NONE => NONE
                                 | SOME sub_result => SOME (word :: sub_result)

fun get_substitutions1 (words_list, target) = 
    case words_list of
        [] => []
      | words :: words_list' => case all_except_option(target, words) of
                                    NONE => get_substitutions1(words_list', target)
                                  | SOME result => result @ get_substitutions1(words_list', target)

fun get_substitutions2 (words_list, target) = 
    let fun f (words_list, acc) =
            case words_list of
                [] => acc
              | words :: words_list' => case all_except_option(target, words) of
                                            NONE => f(words_list', acc)
                                          | SOME result => f(words_list', acc @ result)
    in
        f(words_list, [])
    end

fun similar_names(first_names_list, full_name) = 
    let 
        val {first=x, middle=y, last=z} = full_name
        fun f (names) = 
            case names of 
                [] => []
              | name :: names' => {first=name, middle=y, last=z} :: f(names')
    in
        full_name :: f(get_substitutions2(first_names_list, x))
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

fun card_color(c) = case c of
    (Clubs, _)    => Black
  | (Diamonds, _) => Red
  | (Hearts, _)   => Red
  | (Spades, _)   => Black

fun card_value(c) = case c of
    (_, Num v) => v
  | (_, Jack)  => 10
  | (_, Queen) => 10
  | (_, King)  => 10
  | (_, Ace)   => 11

fun remove_card(cs, t, e) = case cs of
    [] => raise e
  | c :: cs' => if c = t then cs' else c :: remove_card(cs', t, e)

fun all_same_color(cs) = case cs of 
    [] => true
  | _::[] => true
  | x::y::cs' => card_color(x) = card_color(y) andalso all_same_color(y :: cs')

fun sum_cards(cs) = 
    let fun f (cs, acc) = case cs of
        [] => acc
      | c::cs' => f(cs', acc + card_value(c))
    in f(cs, 0)
    end

fun score(cs, goal) = 
    let val sum = sum_cards(cs)
        val ps = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color cs then ps div 2 else ps
    end

fun officiate(cs, ms, goal) =
    let fun f(cs, hs, ms) =
        case ms of 
            [] => score(hs, goal)
          | Draw :: ms' => (case cs of 
                               [] => score(hs, goal)
                             | c :: cs' => if sum_cards(c::hs) > goal 
                                           then score(c::hs, goal)
                                           else f (cs', c::hs, ms'))
          | Discard x :: ms' => f (cs, remove_card(hs, x, IllegalMove), ms')
    in
        f (cs, [], ms)
    end

fun sum_cards_challenge(cs) =
    let fun f (cs, acc) = case cs of
        [] => acc
      | (_, Ace)::cs' => f(cs', acc+1)
      | c::cs' => f(cs', acc + card_value(c))
    in f(cs, 0)
    end

fun ace_count(cs) = 
    let fun f (cs, acc) = 
        case cs of 
            [] => acc
          | (_, Ace)::cs' => f(cs', acc+1)
          | _::cs' => f(cs', acc)
    in f(cs, 0)
    end

fun score_challenge(cs, goal) =
    let val sum = sum_cards_challenge cs
        val cnt = ace_count cs
        val same_color = all_same_color cs
        fun score(sum) = (if sum > goal then 3 * (sum - goal) else goal - sum) div (if same_color then 2 else 1)
        fun min_score (sum, cnt, acc) = 
            let val curr = score(sum)
            in
                if cnt < 0 
                then acc
                else min_score(sum+10, cnt-1, if curr < acc then curr else acc)
            end
    in
        min_score (sum+10, cnt-1, score(sum))
    end

fun officiate_challenge(cs, ms, goal) =
    let fun f(cs, hs, ms) =
        case ms of 
            [] => score_challenge(hs, goal)
          | Draw :: ms' => (case cs of 
                               [] => score_challenge(hs, goal)
                             | c :: cs' => let val sum = sum_cards_challenge(c::hs)
                                           in if sum > goal 
                                              then score_challenge(c::hs, goal)
                                              else f (cs', c::hs, ms')
                                           end)
          | Discard x :: ms' => f (cs, remove_card(hs, x, IllegalMove), ms')
    in
        f (cs, [], ms)
    end

fun careful_player(cs, goal) =
    let 
        fun test(c, hs_left, hs_right) = 
            case hs_right of
                [] => NONE
              | mid::hs_right' => if score(c :: (hs_left @ hs_right'), goal) = 0 
                                  then SOME mid 
                                  else test(c, mid::hs_left, hs_right')
        fun f(cs, hs, ms) =
            if score(hs, goal) = 0
            then ms
            else 
                case cs of 
                    [] => ms @ [Draw]
                  | c::cs' => if ((goal - sum_cards(hs)) > 10)
                              then f(cs', c::hs, ms @ [Draw])
                              else (case test(c, [], hs) of
                                      NONE => (case hs of
                                                    [] => if card_value(c) <= goal then f (cs', c::hs, ms @ [Draw]) else ms
                                                  | h::hs' => f (cs, hs', ms @ [Discard(h)]))
                                    | SOME x => ms @ [Discard(x), Draw])
    in
        f(cs, [], [])
    end
