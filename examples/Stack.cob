// 3-state stack

class Stack
[ New(sender) ▸
    object this : PUSH(?,?,?,?) +
    	          POP(?,?,?) +
      		  EMPTY(?)·(1 + Peek(?) + Push(?,?)) +
      		  SOME(?,?)·(Peek(?) + Push(?,?) + Pop(?)) +
		  FULL(?)·(Peek(?) + Pop(?))
    [ PUSH(a, n, x, sender) ▸
        sender!Reply(this) &
      	Array.Set(a, n, x);
      	if n + 1 = Array.Length(a) then this!FULL(a)
        else this!SOME(a, n + 1)
    | POP(a, n, sender) ▸
        sender!Reply(Value.AsNumber(Array.Get(a, n)), this) &
        if n = 0 then this!EMPTY(a)
	else this!SOME(a, n - 1)
    | EMPTY(a)   & Push(x, sender) ▸ this!PUSH(a, 0, x, sender)
    | SOME(a, n) & Push(x, sender) ▸ this!PUSH(a, n, x, sender)
    | SOME(a, n) & Pop(sender)     ▸ this!POP(a, n, sender)
    | FULL(a)    & Pop(sender)     ▸ this!POP(a, Array.Length(a) - 1, sender)
    | EMPTY(a)   & Peek(sender)    ▸ this!EMPTY(a) & sender!Empty(this)
    | SOME(a, n) & Peek(sender)    ▸ this!SOME(a, n) & sender!Some(this)
    | FULL(a)    & Peek(sender)    ▸ this!FULL(a) & sender!Full(this)
    ]
    this!EMPTY(Array.New(20, 0)) & sender!Reply(this)
]

object User : Phase₁(?,?) + Phase₂(?)
[ Phase₁(n, s) ▸
    let n = n + 1 in
    let s = s.Push(n) in
    System.Print("Push " ^ Value.ToString(n));
    case s.Peek of
    [ Some(s) ▸ User!Phase₁(n, s)
    | Full(s) ▸ User!Phase₂(s) ]
| Phase₂(s) ▸
    let x, s = s.Pop in
    System.Print("Pop " ^ Value.ToString(x));
    case s.Peek of
    [ Empty(s) ▸ User!Phase₁(x, s)
    | Some(s)  ▸ User!Phase₂(s) ]
]

User!Phase₁(0, Stack.New) &
System.Wait(0.5);
done
