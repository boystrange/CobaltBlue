
// A simple future that must be written once and can be read an
// arbitrary number of times, but cannot fail
class Future
[ New(sender) ▸
    object this : (WAIT·Set(?) + DONE(?))·*Get(?)
    [ WAIT        & Set(value)  ▸ this!DONE(value)
    | DONE(value) & Get(sender) ▸ this!DONE(value) & sender!Reply(value) ]

    this!WAIT & sender!Reply(this)
]

class Math
[ Fibo(n, sender) ▸
    if n ≤ 1 then sender!Reply(n)
    else sender!Reply(Math.Fibo(n - 1) + Math.Fibo(n - 2))
]

let x = Future.New in
let y = Future.New in
let z = Future.New in
x!Set(Math.Fibo(22)) &
y!Set(Math.Fibo(23)) &
z!Set(x.Get + y.Get) &
System.Print("Result: " ^ Value.ToString(z.Get)); done &
System.Print("Meanwhile, the main thread goes on!");
System.Wait(20); done
