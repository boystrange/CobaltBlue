
class Session
[ New(sender) ▸
    object this : S₁·Send(?,?)·Receive(?) +
    	   	  S₂·Send(?,?)·Receive(?) +
	   	  S₃·Receive(?)·Send(?,?)
    [ S₁ & Send(n, c⁺) & Receive(c⁻) ▸
        this!S₂ & c⁺!Reply(this) & c⁻!Reply(n, this)
    | S₂ & Send(n, c⁺) & Receive(c⁻) ▸
        this!S₃ & c⁺!Reply(this) & c⁻!Reply(n, this)
    | S₃ & Receive(c⁺) & Send(n, c⁻) ▸
        this!S₁ & c⁺!Reply(n, this) & c⁻!Reply(this) ]

    this!S₁ & sender!Reply(this)
]

class Client
[ Run(c) ▸
    let c = c.Send(Number.Random(10)) in
    let c = c.Send(Number.Random(10)) in
    let n, c = c.Receive in
    System.Print("Client received " ^ Value.ToString(n));
    Client!Run(c)
]

class Service
[ Run(c) ▸
    let n, c = c.Receive in
    let m, c = c.Receive in
    System.Print("Service received " ^ Value.ToString(n)
    				     ^ " and "
				     ^ Value.ToString(m));
    let c = c.Send(n + m) in
    Service!Run(c)
]

let c = Session.New in
Client!Run(c) &
Service!Run(c) &
System.Wait(0.5);
done
