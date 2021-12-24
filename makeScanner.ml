(*

  SCAN. Token scanner for the Pure Lisp interpreter.

    James Moen
    17 Nov 19
    with Madelyn Ogorek's code ogore014

*)

(* TOKEN. A token. *)

type token =
  CloseParenToken |
  EndToken |
  NumberToken of int |
  OpenParenToken |
  SymbolToken of string ;;

(* MAKE SCANNER. Return a function NEXT TOKEN that reads TOKENs from a file
   with pathname PATH. OCaml RAISEs an exception if there's no such file. *)

let makeScanner path =

(* INPUT. Read chars from this channel. *)

  let input = (open_in path)
  in

(* CH. The char most recently read from INPUT. *)

  let ch = ref ' '
  in

(* NEXT CHAR. Advance CH to the next char from INPUT, or to '\000' if we're at
   the end of INPUT. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file -> ch := '\000'
  in

  let nextEndToken () =
    (*return end token*)
    EndToken

  in let nextOpenParenToken () =
    (*advance ch*)
    nextChar ();
    (*return open paren*)
    OpenParenToken

  in let nextCloseParenToken () =
    (*advance ch*)
    nextChar ();
    (*return close paren*)
    CloseParenToken

  in let rec nextNumberToken prefix =
    let temp = ref prefix
    (*matching on the value in ch*)
    in match !ch
    (*if ch is any of these chars, return num token of casted string*)
    with  '\000' -> NumberToken (int_of_string !temp)  |
          '\n'  -> NumberToken (int_of_string !temp) |
          ' ' ->  NumberToken (int_of_string !temp)  |
          '(' ->  NumberToken (int_of_string !temp)  |
          ')' ->  NumberToken (int_of_string !temp)  |
          (*continue until you hit the other cases*)
          _ ->  temp := !temp^ Char.escaped !ch;  (*concatenating the new char onto the existing prefix*)
                    nextChar ();
                    nextNumberToken !temp;


    in let rec nextSymbolToken prefix =
      let temp = ref prefix
      (*matching on the value in ch*)
      in match !ch
      (*if ch is any of these chars, return num token of casted string*)
      with  '\000' -> SymbolToken (!temp)  |
            '\n'  -> SymbolToken (!temp) |
            ' ' ->  SymbolToken (!temp)  |
            '(' ->  SymbolToken (!temp)  |
            ')' ->  SymbolToken (!temp)  |
            (*continue until you hit the other cases*)
            _ ->  temp := !temp^ Char.escaped !ch;  (*concatenating the new char onto the existing prefix*)
                      nextChar ();
                      nextSymbolToken !temp;

    in let nextNumberOrSymbolToken () =
      nextChar ();
      match !ch
      (*check if ch is a number*)
      with '1' -> nextNumberToken "-" |
           '2' -> nextNumberToken "-" |
           '3' -> nextNumberToken "-" |
           '4' -> nextNumberToken "-" |
           '5' -> nextNumberToken "-" |
           '6' -> nextNumberToken "-" |
           '7' -> nextNumberToken "-" |
           '8' -> nextNumberToken "-" |
           '9' -> nextNumberToken "-" |
           '0' -> nextNumberToken "-" |
           (*otherwise its a symbol*)
           _ -> nextSymbolToken "-"


    in let rec nextToken () =
      match !ch
      (*check each of the cases according to the writeup*)
      with '\000' ->  nextEndToken () |
          '\ ' ->  nextChar ();  nextToken ()  |
          '\n'  -> nextChar (); nextToken ()  |
          '(' ->  nextOpenParenToken () |
          ')' ->  nextCloseParenToken ()  |
          '-' ->  nextNumberOrSymbolToken () |
          '0' ->  nextNumberToken ""  |
          '1' ->  nextNumberToken ""  |
          '2' ->  nextNumberToken ""  |
          '3' ->  nextNumberToken ""  |
          '4' ->  nextNumberToken ""  |
          '5' ->  nextNumberToken ""  |
          '6' ->  nextNumberToken ""  |
          '7' ->  nextNumberToken ""  |
          '8' ->  nextNumberToken ""  |
          '9' ->  nextNumberToken ""  |
          _ ->  nextSymbolToken ""








(* Finally initialize CH, and return NEXT TOKEN as promised. *)

  in nextChar () ;
     nextToken ;;

(* NEXT TOKENS. Test the token scanner by reading tokens from the file whose
   pathname is PATH, and writing one-line descriptions of each token. *)

let nextTokens path =
  let nextToken = makeScanner path
  in let rec nextTokensing token =
       match token
       with CloseParenToken ->
              Printf.printf "CloseParenToken\n" ;
              nextTokensing (nextToken ()) |

            EndToken ->
              Printf.printf "EndToken\n" |

            NumberToken number ->
              Printf.printf "NumberToken %i\n" number ;
              nextTokensing (nextToken ()) |

            OpenParenToken ->
              Printf.printf "OpenParenToken\n" ;
              nextTokensing (nextToken ()) |

            SymbolToken string ->
              Printf.printf "SymbolToken \"%s\"\n" string ;
              nextTokensing (nextToken ())

     in nextTokensing (nextToken ()) ;;

(* Try reading tokens from EXCLAIM. *)

nextTokens "exclaim" ;;

(* the following is printed:

   OpenParenToken
   SymbolToken "define"
   SymbolToken "!"
   OpenParenToken
   SymbolToken "lambda"
   OpenParenToken
   SymbolToken "n"
   CloseParenToken
   OpenParenToken
   SymbolToken "if"
   OpenParenToken
   SymbolToken "="
   SymbolToken "n"
   NumberToken 0
   CloseParenToken
   NumberToken 1
   OpenParenToken
   SymbolToken "∗"
   SymbolToken "n"
   OpenParenToken
   SymbolToken "!"
   OpenParenToken
   SymbolToken "−"
   SymbolToken "n"
   NumberToken 1
   CloseParenToken
   CloseParenToken
   CloseParenToken
   CloseParenToken
   CloseParenToken
   CloseParenToken
   EndToken

*)

(* results of running tests:
Test cases found:                                                                   + symbols.in                                                                    + parens_spaced.in
    + parens.in
    + exclaim.in
    + num_list.in
Your output files will be:
    - symbols.out
    - parens_spaced.out
    - parens.out
    - exclaim.out
    - num_list.out
   in ./your_output

Beginning the run-through of all files...

Beginning parsing of ./cases/symbols.in...
  Written to ./your_output/symbols.out!
Beginning parsing of ./cases/parens_spaced.in...
  Written to ./your_output/parens_spaced.out!
Beginning parsing of ./cases/parens.in...
  Written to ./your_output/parens.out!
Beginning parsing of ./cases/exclaim.in...
  Written to ./your_output/exclaim.out!
Beginning parsing of ./cases/num_list.in...
  Written to ./your_output/num_list.out!

All tests finished!
Running diff on ouput...

No differences between ./your_output/symbols.out and ./expected/symbols.out
No differences between ./your_output/parens_spaced.out and ./expected/parens_spaced.out
No differences between ./your_output/parens.out and ./expected/parens.out
No differences between ./your_output/exclaim.out and ./expected/exclaim.out
No differences between ./your_output/num_list.out and ./expected/num_list.out

Testing script finished!
- : unit = ()
*)
