(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
use "hw2solution.sml";
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = all_except_option ("string", ["string"]) = SOME []

val test1_1 = all_except_option ("target", ["not-target", "not-target-2", "target", "not-target-3"]) = SOME ["not-target", "not-target-2", "not-target-3"]

val test1_2 = all_except_option ("target", ["not-target", "not-target-2"]) = NONE

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test2_1 = get_substitutions1 ([["foo", "1"], ["not1", "not2"], ["foo", "2"], ["3", "foo"]], "foo") = ["1", "2", "3"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test3_1 = get_substitutions2 ([["foo", "1"], ["not1", "not2"], ["foo", "2"], ["3", "foo"]], "foo") = ["1", "2", "3"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test7_1 = remove_card ([(Hearts, Ace), (Hearts, Num 11), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Num 11), (Hearts, Ace)]

val test7_2 = let val v = remove_card ([(Hearts, Num 11)], (Hearts, Ace), IllegalMove) in false end handle IllegalMove => true

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

val test14_1 = score_challenge([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test14_2 = score_challenge([(Hearts, Num 2),(Clubs, Num 4),(Clubs, Ace)],10) = 3

val test14_3 = score_challenge([(Clubs, Ace)],10) = 1

val test15_1 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test15_2 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test15_3 = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

val test16 = careful_player([(Clubs,Num(1)),(Spades,Num(8))],8)
