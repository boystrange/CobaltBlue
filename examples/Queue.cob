// Node interface
type #Last = Link(#Any)·#Any
and  #Next = Unlink(Reply(#Any)) + #Any
and  #Any  = GetData(Reply(#Number, #Any)) +
     	     HasNext(False(#Any) + True(#Next))
// Queue interface
and  #Enqueue = Enqueue(#Number, Reply(#Enqueue))
and  #Dequeue = Dequeue(None(#Dequeue) + Some(#Number, #Dequeue))
in

class Node : *New(#Number, Reply(#Last))
[ New(x, sender) ▸
    object this : DATA(#Number)·(1 + LAST·#Last + NEXT(#Any)·#Next)
    [ DATA(x)    & GetData(sender) ▸ this!DATA(x) & sender!Reply(x, this)
    | LAST       & HasNext(sender) ▸ this!LAST & sender!False(this)
    | NEXT(node) & HasNext(sender) ▸ this!NEXT(node) & sender!True(this)
    | LAST       & Link(node)      ▸ this!NEXT(node)
    | NEXT(node) & Unlink(sender)  ▸ sender!Reply(node) ]

    this!DATA(x) & this!LAST & sender!Reply(this)
]

class Queue : *New(Reply(#Enqueue·#Dequeue))
[ New(sender) ▸
    object this : HEAD(#Any)·TAIL(Link(#Any))·#Enqueue·#Dequeue
    [ TAIL(tail) & Enqueue(x, sender) ▸
        let node = Node.New(x) in
	this!TAIL(node) & tail!Link(node) & sender!Reply(this)
    | HEAD(head) & Dequeue(sender) ▸
        case head.HasNext of
	[ False(head) ▸ this!HEAD(head) & sender!None(this)
	| True(head)  ▸ let head = head.Unlink in
	  	      	let x, head = head.GetData in
		      	this!HEAD(head) & sender!Some(x, this) ]
    ]

    let node = Node.New(0) in
    this!HEAD(node) & this!TAIL(node) & sender!Reply(this)
]

class Producer
[ Run(queue, n) ▸
    System.Wait(Number.Random(2));
    System.Print("→ " ^ Value.ToString(n));
    let queue = queue.Enqueue(n) in
    Producer!Run(queue, n + 1) ]

class Consumer
[ Run(queue) ▸
    System.Wait(Number.Random(2));
    case queue.Dequeue of
    [ None(queue)    ▸ Consumer!Run(queue)
    | Some(n, queue) ▸ System.Print(Value.ToString(n) ^ " ←");
      	      	       Consumer!Run(queue) ] ]

let queue = Queue.New in
Producer!Run(queue, 0) &
Consumer!Run(queue) &
System.Wait(20);
done

