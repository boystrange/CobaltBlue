
type #Iterator = Peek(None(#None) + Some(#Some))
and  #None     = Peek(None(#None)) + 1
and  #Some     = Peek(Some(#Some)) + Next(Reply(#Number, #Iterator)) in

class Iterator : *New(#Array, Reply(#Iterator))
[ New(a, sender) ▸
    object this : Init(#Array, #Number)·#Iterator +
    	   	  NONE·#None + SOME(#Array, #Number)·#Some
    [ Init(a, i) ▸
	if i < Array.Length(a) then this!SOME(a, i)
	else this!NONE
    | NONE       & Peek(sender) ▸ this!NONE & sender!None(this)
    | SOME(a, i) & Peek(sender) ▸ this!SOME(a, i) & sender!Some(this)
    | SOME(a, i) & Next(sender) ▸
	this!Init(a, i + 1) &
	sender!Reply(Value.AsNumber(Array.Get(a, i)), this) ]

    this!Init(a, 0) & sender!Reply(this)
]

object User : Init(?,?,?) + Loop(#Iterator, ?) + 1
[ Init(a, i, sender) ▸
    if i < Array.Length(a) then
      Array.Set(a, i, Number.Random(100));
      User!Init(a, i + 1, sender)
    else
      User!Loop(Iterator.New(a), sender)
| Loop(it, sender) ▸
    case it.Peek of
    [ None(it) ▸ case it.Peek of // the second Peek in a row is more precise
      	       	 [ None(it) ▸ sender!Reply ]
    | Some(it) ▸ case it.Peek of // the second Peek in a row is more precise
      		 [ Some(it) ▸ let x, it = it.Next in
		      	      System.Print("User reads " ^ Value.ToString(x));
		      	      User!Loop(it, sender) ] ]
]

User.Init(Array.New(10, 0), 0);
done
