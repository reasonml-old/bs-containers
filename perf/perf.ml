
external time : string -> unit = "console.time" [@@bs.val]
external timeEnd : string -> unit = "console.timeEnd" [@@bs.val]

let rec repeat f num = 
  if num = 0 then ()
  else (
    f ();
    repeat f (num - 1)
  )

let defaultRepeatTimes = 1000000

let runPerfTest name ?(repeatTimes = defaultRepeatTimes) f = 
  time name;
  repeat f repeatTimes;
  timeEnd name