#use "ex00/ft_test_sign.ml"
#use "ex01/ft_countdown.ml"
#use "ex02/ft_power.ml"
#use "ex03/ft_print_alphabet.ml"
#use "ex04/ft_print_comb.ml"
#use "ex05/ft_print_rev.ml"
#use "ex06/ft_string_all.ml"
#use "ex07/ft_is_palindrome.ml"
#use "ex08/ft_rot_n.ml"
#use "ex09/ft_print_comb2.ml"

let print_green_string s =
    print_string "\x1b[32m"
    ; print_string s
    ; print_endline "\x1b[0m"

let print_red_string s =
    print_string "\x1b[31m"
    ; print_string s
    ; print_endline "\x1b[0m"

let test_ex00 () =
    print_green_string "EX00 - FT_TEST_SIGN"
    ; print_green_string "-------------------\n"
    ; ft_test_sign 0
    ; ft_test_sign 42
    ; ft_test_sign max_int
    ; ft_test_sign (-422)
    ; ft_test_sign min_int

let test_ex01 () =
    print_green_string "EX01 - FT_COUNTDOWN"
    ; print_green_string "-------------------\n"
    ; print_endline "Countdown from -1: "
    ; ft_countdown (-1)
    ; print_endline "Countdown from 0: "
    ; ft_countdown 0
    ; print_endline "Countdown from 1: "
    ; ft_countdown 1
    ; print_endline "Countdown from 5: "
    ; ft_countdown 5

let test_ex02 () =
    print_green_string "EX02 - FT_POWER"
    ; print_green_string "---------------\n"
    ; let test_power a b truth =
        let p = ft_power a b in
            print_string "ft_power "
            ; print_int a
            ; print_char ' '
            ; print_int b
            ; print_string " = "
            ; print_int p
            ; if p == truth then
                print_green_string " | VALID"
            else
                begin
                    print_red_string " | INVALID"
                    ; print_string "      -> truth = "
                    ; print_int truth
                    ; print_char '\n'
                end
    in
    test_power 2 4 16
    ; test_power 3 0 1
    ; test_power 0 5 0
    ; test_power 2 5 32
    ; test_power 2 10 1024
    ; test_power (-10) 0 1
    ; test_power (-3) 3 (-27)

let test_ex03 () =
    print_green_string "EX03 - FT_PRINT_ALPHABET"
    ; print_green_string "------------------------\n"
    ; ft_print_alphabet ()

let test_ex04 () =
    print_green_string "EX04 - FT_PRINT_COMB"
    ; print_green_string "--------------------\n"
    ; ft_print_comb ()

let test_ex05 () =
    print_green_string "EX05 - FT_PRINT_REV"
    ; print_green_string "-------------------\n"
    ; print_string "Hello world ! \n-> "
    ; ft_print_rev "\"Hello world !\""
    ; print_string "[EMPTY] \n-> "
    ; ft_print_rev ""
    ; print_string "\"U\" \n-> "
    ; ft_print_rev "U"
    ; print_string "\"J'ai fait un gros fichier de test pour te faciliter la vie :)\" \n-> "
    ; ft_print_rev "J'ai fait un gros fichier de test pour te faciliter la vie :)"

let test_ex06 () =
    print_green_string "EX06 - FT_STRING_ALL"
    ; print_green_string "--------------------\n"
    ; let is_digit c = begin
        let zero = int_of_char '0' in
        let nine = int_of_char '9' in
        let current = int_of_char c in
        (current <= nine && current >= zero)
    end in
    let test predicate s =
        print_endline ("ft_string_all is_digit \"" ^ s ^ "\" => " ^ (string_of_bool (ft_string_all is_digit s)))
    in
    test is_digit "01273649824529842380589258238923023048923751832589235923587239857239849238"
    ; test is_digit "01273649824529842380589258238923 023048923751832589235923587239857239849238"
    ; test is_digit ""
    ; test is_digit "_"
    ; test is_digit "0"

let test_ex07 () =
    print_green_string "EX07 - FT_IS_PALINDROME"
    ; print_green_string "-----------------------\n"
    ; let test s =
        print_endline (s ^ " => " ^ (string_of_bool (ft_is_palindrome s)))
    in
    test "car"
    ; test "hello"
    ; test ""
    ; test "ete"
    ; test "kayak"
    ; test "radar"
    ; test "ra dar"
    ; test "r"

let test_ex08 () =
    print_green_string "EX08 - FT_ROT_N"
    ; print_green_string "---------------\n"
    ; let test n s =
        print_endline ("ft_rot_n " ^ (string_of_int n) ^ " \"" ^ s ^ "\" = " ^ (ft_rot_n n s))
    in
    test 0 "abc"
    ; test 1 "abc"
    ; test 26 "abc"
    ; test 25 "abc"
    ; test 12395238 "0123456789 abcdef  ABCDEF ! * % - + ' |"
    ; test 42 ""
    ; test 42 "!"
    ; test 1 "NBzlk qnbjr !"

let test_ex09 () =
    print_green_string "EX09 - FT_PRINT_COMB2"
    ; print_green_string "---------------------\n"
    ; ft_print_comb2 ()

let () =
    test_ex00 ()
    ; print_char '\n'
    ; test_ex01 ()
    ; print_char '\n'
    ; test_ex02 ()
    ; print_char '\n'
    ; test_ex03 ()
    ; print_char '\n'
    ; test_ex04 ()
    ; print_char '\n'
    ; test_ex05 ()
    ; print_char '\n'
    ; test_ex06 ()
    ; print_char '\n'
    ; test_ex07 ()
    ; print_char '\n'
    ; test_ex08 ()
    ; print_char '\n'
    ; test_ex09 ()
    ; print_char '\n'
