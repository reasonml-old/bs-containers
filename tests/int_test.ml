open InfiniteJest
open InfiniteJest.Test
open Int

let suite = 
  describe "Int" (fun () -> [
		test "neg - 42" (fun () ->
			42 |> neg 
			   |> Expect.toEqual (~-42)
		  );

		test "hash" (fun () ->
			42 |> hash 
			   |> Expect.toEqual 42
		  );
		test "hash - 0" (fun () ->
			0 |> hash
			  |> Expect.toEqual 0
		  );
		test "hash - minValue" (fun () -> 
			minValue |> hash
			         |> Expect.toEqual 0
		  );

		test "compare - Less" (fun () ->
			Int.compare 1 2
			|> Expect.toEqual Ordering.Less
		  );
		test "compare - Greater" (fun () ->
			Int.compare 2 1
			|> Expect.toEqual Ordering.Greater
		  );
		test "compare - Equal" (fun () ->
			Int.compare 2 2
			|> Expect.toEqual Ordering.Equal
		  );

		test "equals - true" (fun () ->
			Int.equals 2 2
			|> Expect.toBeTrue
		  );
		test "equals - false" (fun () ->
			Int.equals 2 3
			|> Expect.toBeFalse
		  );

		test "neg - (-42)" (fun () ->
			(~-42) |> neg
			       |> Expect.toEqual 42
		  );
		test "neg - minValue" (fun () ->
			minValue |> neg 
			         |> Expect.toEqual minValue
		  );
		test "neg - maxValue" (fun () -> 
			maxValue |> neg
			         |> Expect.toEqual (minValue + 1)
		  );

		test "pow" (fun () -> 
			pow 2 1 
			|> Expect.toEqual 2
		  );
		test "pow - 2^2" (fun () ->
			pow 2 2
			|> Expect.toEqual 4
		  );
		test "pow - 3 ^ 10" (fun () ->
			pow 3 10 
			|> Expect.toEqual 59049
		  ); 
		test "pow - overflow" (fun () ->
			pow 2 32
			|> Expect.toEqual 0
		  );
		test "pow - raise" (fun () ->
			(fun () -> pow 0 0) 
			|> Expect.toRaise
		  );
		test "pow - negative" (fun () -> 
			(fun () -> pow 1 (~-1)) 
			|> Expect.toRaise
		  );

		test "sign - positive" (fun () ->
			42 |> sign 
			   |> Expect.toEqual 1
		  );
		test "sign - zero" (fun () ->
			0 |> sign
			  |> Expect.toEqual 0
		  );
		test "sign - negative" (fun () ->
			(~-42) |> sign
			       |> Expect.toEqual (~-1)
		  );

		test "toString" (fun () -> 
			42 |> toString
			   |> Expect.toEqual "42"
		  );

		test "fromString" (fun () ->
			"233" |> fromString
			      |> Expect.toEqual (Some 233)
		  );
		test "fromString - Empty" (fun () ->
			"" |> fromString
			   |> Expect.toEqual None
		  );
		test "fromString - None" (fun () ->
			"23a" |> fromString
			      |> Expect.toEqual None
		  );
		test "fromString - Overflow" (fun () ->
			"2147483648" |> fromString
			             |> Expect.toEqual None
		  );

		test "toBinaryString - 1" (fun () ->
			1 |> toBinaryString 
			  |> Expect.toEqual "0b1"
		  );
		test "toBinaryString - 32" (fun () ->
			32 |> toBinaryString 
			   |> Expect.toEqual "0b100000"
		  );
		test "toBinaryString - (-2)" (fun () -> 
			(~-2) |> toBinaryString
			      |> Expect.toEqual "-0b10"
		  );
		test "toBinaryString - maxValue" (fun () ->
			maxValue |> toBinaryString 
			         |> Expect.toEqual "0b1111111111111111111111111111111"
		  );
		test "toBinaryString - minValue" (fun () -> 
			minValue |> toBinaryString
			         |> Expect.toEqual "-0b10000000000000000000000000000000"
		  );

		test "min" (fun () -> 
			Int.min 1 2 
			|> Expect.toEqual 1
		  );
		test "min - eq" (fun () ->
			Int.min 1 1
			|> Expect.toEqual 1
		  );

		test "max" (fun () -> 
			Int.max 1 2 
			|> Expect.toEqual 2
		  );
		test "max - eq" (fun () -> 
			Int.max 1 1 
			|> Expect.toEqual 1
		  )
	  ])
