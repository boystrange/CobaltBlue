
class NonEmptyStack
[ New(x, s, r) ▸
    object this : STACK(?,?)·(Empty(?) + Push(?,?) + Pop(?)) + 1
    [ Empty(r)             ▸ r!False(this)
    | Push(x, r)           ▸ r!Reply(NonEmptyStack.New(x, this))
    | STACK(x, s) & Pop(r) ▸ r!Reply(x, s) ]
    this!STACK(x, s) & r!Reply(this)
]

class Stack
[ New(r) ▸
    object this : Empty(?) + Push(?,?) + 1
    [ Empty(r)   ▸ r!True(this)
    | Push(x, r) ▸ r!Reply(NonEmptyStack.New(x, this)) ]
    r!Reply(this)
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
