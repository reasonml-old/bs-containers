
let _ =
  InfiniteJest.Test.run [
    Bchar_test.suite;
    Bmap_test.suite;
    Bool_test.suite;
    Bopt_test.suite;
    Bresult_test.suite;
    Int_test.suite;
    Iterator_test.suite
  ]