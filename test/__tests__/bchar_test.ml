open Test

let _ = 
  test "lowercaseIfAscii" (fun t ->
      deepEqual t (Bchar.lowerCaseIfAscii 'A') 'a';
      deepEqual t (Bchar.lowerCaseIfAscii '0') '0';
    );

  test "fromInt" (fun t ->
      let c = Bchar.fromInt 48 in
      deepEqual t c (Some '0');
      deepEqual t (Bchar.fromInt 256) None;
    );

  test "fromIntOrRaise" (fun t ->
      ignore @@ (try ignore @@ Bchar.fromIntOrRaise 256
                 with 
                 | Invalid_argument _ -> pass t 
                 | _ -> fail t);
      ignore @@ (try ignore @@ Bchar.fromIntOrRaise (~-1)
                 with
                 | Invalid_argument _ -> pass t
                 | _ -> fail t);
    );

  test "toInt" (fun t ->
      deepEqual t (Bchar.toInt 'a') 97
    );

  test "isDigit" (fun t ->
      deepEqual t (Bchar.isDigit '0') true;
      deepEqual t (Bchar.isDigit 'a') false
    );

  test "isUpperCase" (fun t ->
      deepEqual t (Bchar.isUpperCase '0') false;
      deepEqual t (Bchar.isUpperCase 'a') false;
      deepEqual t (Bchar.isUpperCase 'A') true
    );

  test "isLowerCase" (fun t ->
      deepEqual t (Bchar.isLowerCase '0') false;
      deepEqual t (Bchar.isLowerCase 'a') true;
      deepEqual t (Bchar.isLowerCase 'A') false
    );

  test "isLetter" (fun t ->
      deepEqual t (Bchar.isLetter '0') false;
      deepEqual t (Bchar.isLetter 'a') true;
      deepEqual t (Bchar.isLetter 'A') true
    );

  test "isLetterOrDigit" (fun t ->
      deepEqual t (Bchar.isLetterOrDigit '\000') false;
      deepEqual t (Bchar.isLetterOrDigit '0') true;
      deepEqual t (Bchar.isLetterOrDigit 'a') true;
      deepEqual t (Bchar.isLetterOrDigit 'A') true
    );

  test "isWhitespace" (fun t -> 
      let open Bchar in 
      deepEqual t (isWhitespace '\008') false;
      deepEqual t (isWhitespace '\009') true;
      deepEqual t (isWhitespace '\010') true;
      deepEqual t (isWhitespace '\011') true;
      deepEqual t (isWhitespace '\012') true;
      deepEqual t (isWhitespace '\013') true;
      deepEqual t (isWhitespace '\014') false;
      deepEqual t (isWhitespace '\032') true
    );
