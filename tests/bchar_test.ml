open InfiniteJest
open InfiniteJest.Test
open Bchar

let suite = 
  describe "Bchar" (fun () -> [
    test "lowerCaseIfAscii - 'A'" (fun () ->
      'A' |> lowerCaseIfAscii
          |> Expect.toEqual 'a');
    test "lowerCaseIfAscii - '0'" (fun () ->
      '0' |> lowerCaseIfAscii
          |> Expect.toEqual '0');

    test "fromInt - 48" (fun () ->
      48 |> fromInt
         |> Expect.toEqual (Some '0'));
    test "fromInt - 256" (fun () ->
      256 |> fromInt
          |> Expect.toEqual None);

    test "fromIntOrRaise - 48" (fun () ->
      48 |> fromIntOrRaise
         |> Expect.toEqual '0');
    test "fromIntOrRaise - 256" (fun () ->
      (fun () ->
        256 |> fromIntOrRaise)
            |> Expect.toRaise);
    test "fromIntOrRaise - (~-1)" (fun () ->
      (fun () ->
        (~-1) |> fromIntOrRaise)
              |> Expect.toRaise);

    test "toInt" (fun () ->
      'a' |> toInt
          |> Expect.toEqual 97);

    test "isDigit - '0'" (fun () ->
      '0' |> isDigit
          |> Expect.toBeTrue);
    test "isDigit - 'a'" (fun () ->
      'a' |> isDigit
          |> Expect.toBeFalse);

    test "isUpperCase - '0'" (fun () ->
      '0' |> isUpperCase
          |> Expect.toBeFalse);
    test "isUpperCase - 'a'" (fun () ->
      'a' |> isUpperCase
          |> Expect.toBeFalse);
    test "isUpperCase - 'A'" (fun () ->
      'A' |> isUpperCase
          |> Expect.toBeTrue);

    test "isLowerCase - '0'" (fun () ->
      '0' |> isLowerCase
          |> Expect.toBeFalse);
    test "isLowerCase - 'a'" (fun () ->
      'a' |> isLowerCase
          |> Expect.toBeTrue);
    test "isLowerCase - 'A'" (fun () ->
      'A' |> isLowerCase
          |> Expect.toBeFalse);

    test "isLetter - '0'" (fun () ->
      '0' |> isLetter
          |> Expect.toBeFalse);
    test "isLetter - 'a'" (fun () ->
      'a' |> isLetter
          |> Expect.toBeTrue);
    test "isLetter - 'A'" (fun () ->
      'A' |> isLetter
          |> Expect.toBeTrue);

    test "isLetterOrDigit - '\\000'" (fun () ->
      '\000' |> isLetterOrDigit
             |> Expect.toBeFalse);
    test "isLetterOrDigit - '0'" (fun () ->
      '0' |> isLetterOrDigit
          |> Expect.toBeTrue);
    test "isLetterOrDigit - 'a'" (fun () ->
      'a' |> isLetterOrDigit
          |> Expect.toBeTrue);
    test "isLetterOrDigit - 'A'" (fun () ->
      'A' |> isLetterOrDigit
          |> Expect.toBeTrue);

    test "isWhitespace - '\\008'" (fun () ->
      '\008' |> isWhitespace
             |> Expect.toBeFalse);
    test "isWhitespace - '\\009'" (fun () ->
      '\009' |> isWhitespace
             |> Expect.toBeTrue);
    test "isWhitespace - '\\010'" (fun () ->
      '\010' |> isWhitespace
             |> Expect.toBeTrue);
    test "isWhitespace - '\\011'" (fun () ->
      '\011' |> isWhitespace
             |> Expect.toBeTrue);
    test "isWhitespace - '\\012'" (fun () ->
      '\012' |> isWhitespace
             |> Expect.toBeTrue);
    test "isWhitespace - '\\013'" (fun () ->
      '\013' |> isWhitespace
             |> Expect.toBeTrue);
    test "isWhitespace - '\\014'" (fun () ->
      '\014' |> isWhitespace
             |> Expect.toBeFalse);
    test "isWhitespace - '\\032'" (fun () ->
      '\032' |> isWhitespace
             |> Expect.toBeTrue);
  ])