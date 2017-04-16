open InfiniteJest
open InfiniteJest.Test
open Bchar

let suite = 
  describe "Bchar" (fun () -> [
    test "lowerCaseIfAscii - 'A'" (fun () ->
      lowerCaseIfAscii 'A' |> Expect.toEqual 'a');
    test "lowerCaseIfAscii - '0'" (fun () ->
      lowerCaseIfAscii '0' |> Expect.toEqual '0');

    test "fromInt - 48" (fun () ->
      fromInt 48 |> Expect.toEqual (Some '0'));
    test "fromInt - 256" (fun () ->
      fromInt 256 |> Expect.toEqual None);

    test "fromIntOrRaise - 48" (fun () ->
      fromIntOrRaise 48 |> Expect.toEqual '0');
    test "fromIntOrRaise - 256" (fun () ->
      (fun () -> fromIntOrRaise 256) |> Expect.toRaise);
    test "fromIntOrRaise - (~-1)" (fun () ->
      (fun () -> fromIntOrRaise (~-1)) |> Expect.toRaise);

    test "toInt" (fun () ->
      toInt 'a' |> Expect.toEqual 97);

    test "isDigit - '0'" (fun () ->
      isDigit '0' |> Expect.toBeTrue);
    test "isDigit - 'a'" (fun () ->
      isDigit 'a' |> Expect.toBeFalse);

    test "isUpperCase - '0'" (fun () ->
      isUpperCase '0' |> Expect.toBeFalse);
    test "isUpperCase - 'a'" (fun () ->
      isUpperCase 'a' |> Expect.toBeFalse);
    test "isUpperCase - 'A'" (fun () ->
      isUpperCase 'A' |> Expect.toBeTrue);

    test "isLowerCase - '0'" (fun () ->
      isLowerCase '0' |> Expect.toBeFalse);
    test "isLowerCase - 'a'" (fun () ->
      isLowerCase 'a' |> Expect.toBeTrue);
    test "isLowerCase - 'A'" (fun () ->
      isLowerCase 'A' |> Expect.toBeFalse);

    test "isLetter - '0'" (fun () ->
      isLetter '0' |> Expect.toBeFalse);
    test "isLetter - 'a'" (fun () ->
      isLetter 'a' |> Expect.toBeTrue);
    test "isLetter - 'A'" (fun () ->
      isLetter 'A' |> Expect.toBeTrue);

    test "isLetterOrDigit - '\\000'" (fun () ->
      isLetterOrDigit '\000' |> Expect.toBeFalse);
    test "isLetterOrDigit - '0'" (fun () ->
      isLetterOrDigit '0' |> Expect.toBeTrue);
    test "isLetterOrDigit - 'a'" (fun () ->
      isLetterOrDigit 'a' |> Expect.toBeTrue);
    test "isLetterOrDigit - 'A'" (fun () ->
      isLetterOrDigit 'A' |> Expect.toBeTrue);

    test "isWhitespace - '\\008'" (fun () ->
      isWhitespace '\008' |> Expect.toBeFalse);
    test "isWhitespace - '\\009'" (fun () ->
      isWhitespace '\009' |> Expect.toBeTrue);
    test "isWhitespace - '\\010'" (fun () ->
      isWhitespace '\010' |> Expect.toBeTrue);
    test "isWhitespace - '\\011'" (fun () ->
      isWhitespace '\011' |> Expect.toBeTrue);
    test "isWhitespace - '\\012'" (fun () ->
      isWhitespace '\012' |> Expect.toBeTrue);
    test "isWhitespace - '\\013'" (fun () ->
      isWhitespace '\013' |> Expect.toBeTrue);
    test "isWhitespace - '\\014'" (fun () ->
      isWhitespace '\014' |> Expect.toBeFalse);
    test "isWhitespace - '\\032'" (fun () ->
      isWhitespace '\032' |> Expect.toBeTrue);
  ])