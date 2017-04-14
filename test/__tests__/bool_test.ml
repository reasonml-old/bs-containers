open Test

let _ = 
    test "equal" (fun t -> 
        deepEqual t (Bool.equals true true) true;
        deepEqual t (Bool.equals true false) false;
        deepEqual t (Bool.equals false false) true;
    );

    test "compare" (fun t -> 
        deepEqual t (Bool.compare true false) Ordering.Greater;
        deepEqual t (Bool.compare false true) Ordering.Less;
        deepEqual t (Bool.compare false false) Ordering.Equal;
        deepEqual t (Bool.compare true true) Ordering.Equal;
    )