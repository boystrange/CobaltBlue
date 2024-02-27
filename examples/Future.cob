// A future that must be written once, can be read an arbitrary number
// of times, and supports cancellation and timeouts

class Proxy
[ New(future, time, client) ▸
    object this : WAIT(?)·(Done(?) + Cancelled)·Timeout +
      	   	  DONE·Timeout + TIMEOUT·(Done(?) + Cancelled) + 1
    [ WAIT(sender) & Done(value) ▸ this!DONE & sender!Done(value)
    | WAIT(sender) & Cancelled   ▸ this!DONE & sender!Cancelled
    | WAIT(sender) & Timeout     ▸ this!TIMEOUT & sender!Timeout
    | DONE         & Timeout     ▸ done
    | TIMEOUT      & Done(value) ▸ done
    | TIMEOUT      & Cancelled   ▸ done ]

    this!WAIT(client) & future!Get(this) & { System.Wait(time); this!Timeout }
]

class Future
[ New(sender) ▸
    object this : (WAIT·(Set(?) + Cancel) + DONE(?) + CANCELLED)·*Get(?)·*Get(?,?)
    [ WAIT        & Set(value)        ▸ this!DONE(value)
    | WAIT        & Cancel            ▸ this!CANCELLED
    | DONE(value) & Get(sender)       ▸ this!DONE(value) & sender!Done(value)
    | CANCELLED   & Get(sender)       ▸ this!CANCELLED & sender!Cancelled
    | DONE(value) & Get(time, sender) ▸ this!DONE(value) & sender!Done(value)
    | CANCELLED   & Get(time, sender) ▸ this!CANCELLED & sender!Cancelled
    | WAIT        & Get(time, sender) ▸ this!WAIT & Proxy!New(this, time, sender) ]

    this!WAIT & sender!Reply(this)
]

class Math
[ Fibo(n, sender) ▸
    if n ≤ 1 then sender!Reply(n)
    else sender!Reply(Math.Fibo(n - 1) + Math.Fibo(n - 2))
| ElapsedSince(start, sender) ▸
    let end = System.Time in
    sender!Reply(Number.Round(1000 × System.Duration(end, start)))
]

class Main
[ Run(future, sender) ▸
    case future.Get(1) of
    [ Done(result) ▸ System.Print(result); sender!Reply
    | Timeout      ▸ System.Print("Timed out!"); sender!Reply
    | Cancelled    ▸ System.Print("Cancelled!"); sender!Reply ]
]

let future = Future.New in
let n = Number.Random(0, 30) in {
  if Number.Random(4) = 0 then future!Cancel
  else future!Set(Math.Fibo(n))
} & {
  System.Print("Wait for Fibo(" ^ Value.ToString(n) ^ ")");
  let start = System.Time in
  Main.Run(future);
  System.Print("Elapsed time = " ^ Value.ToString(Math.ElapsedSince(start)) ^ " ms");
  done
}
