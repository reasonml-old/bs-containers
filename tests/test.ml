
let _ =
  InfiniteJest.Test.run [
    Bchar_test.suite;
    Bmap_test.suite;
    Bool_test.suite;
    Bopt_test.suite;
    Bresult_test.suite;
  ]