
class Channel
[ New(sender) ▸
    object this : NEW·(Send(?) + Send(?,?))·Receive(?) + USED(?)·Receive(?) + 1
    [ NEW & Send(msg) ▸ this!USED(msg)
    | NEW & Send(msg, sender) & Receive(receiver) ▸
        sender!Reply & receiver!Reply(msg)
    | USED(msg) & Receive(receiver) ▸ receiver!Reply(msg) ]

    this!NEW & sender!Reply(this) ]

class Message
[ New(payload, k, sender) ▸
    linear object this : Read(?) + 1
    [ Read(sender) ▸ sender!Reply(payload, k) ]
    sender!Reply(this) ]

class Sender
[ Run(n, channel) ▸
    if Number.Random(5) = 0 then Sender!Synchronous(n, channel)
    else Sender!Asynchronous(n, channel)
| Synchronous(n, channel) ▸
    System.Print("Sync Send " ^ Value.ToString(n));
    let k = Channel.New in
    channel.Send(Message.New(n, k));
    Sender!Run(n + 1, k)
| Asynchronous(n, channel) ▸
    System.Print("Async Send " ^ Value.ToString(n));
    let k = Channel.New in
    channel!Send(Message.New(n, k)) & Sender!Run(n + 1, k) ]

class Receiver
[ Run(channel) ▸
    System.Wait(Number.Random(10) / 5);
    let n, k = channel.Receive.Read in
    System.Print("Receiver " ^ Value.ToString(n));
    Receiver!Run(k) ]

let channel = Channel.New in
Sender!Run(0, channel) &
Receiver!Run(channel) &
System.Wait(60); done