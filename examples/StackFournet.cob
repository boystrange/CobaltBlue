// 2-state stack à la Fournet

type #Stack         = Push(#Number, Reply(#NonEmptyStack))
     	            + Empty(True(#EmptyStack) + False(#NonEmptyStack))
and  #EmptyStack    = Push(#Number, Reply(#NonEmptyStack))
     		    + Empty(True(#EmptyStack))
		    + 1
and  #NonEmptyStack = Push(#Number, Reply(#NonEmptyStack))
     		    + Empty(False(#NonEmptyStack))
		    + Pop(Reply(#Number, #Stack))
in

class Stack
[ New(r) ▸
    object this : NONE·#EmptyStack + SOME(#Number, #Stack)·#NonEmptyStack
    [ NONE       & Empty(r)   ▸ this!NONE & r!True(this)
    | SOME(x, s) & Empty(r)   ▸ this!SOME(x, s) & r!False(this)
    | NONE       & Push(x, r) ▸ this!SOME(x, Stack.New) & r!Reply(this)
    | SOME(x, s) & Push(y, r) ▸ this!SOME(y, s.Push(x)) & r!Reply(this)
    | SOME(x, s) & Pop(r) ▸
        case s.Empty of
    	[ True(s)  ▸ this!NONE & r!Reply(x, this)
    	| False(s) ▸ let y, s = s.Pop in
      	       	     this!SOME(y, s) & r!Reply(x, this) ]
    ]
    this!NONE & r!Reply(this)
]

object User : Phase₁(?,?) + Phase₂(?) + 1
[ Phase₁(n, s) ▸
    if n > 0 then
      System.Print("Push " ^ Value.ToString(n));
      User!Phase₁(n - 1, s.Push(n))
    else
      User!Phase₂(s)
| Phase₂(s) ▸
    case s.Empty of
    [ True(s)  ▸ done
    | False(s) ▸ let x, s = s.Pop in
      	       	 System.Print("Pop " ^ Value.ToString(x));
		 User!Phase₂(s) ]
]

User!Phase₁(10, Stack.New) &
System.Wait(5);
done