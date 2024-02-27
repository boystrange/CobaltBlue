// linear buffer with class, used forever

type #Insert = 1 + Insert(#Number, Reply(#Remove))
and  #Remove = Remove(Reply(#Number, #Insert))
in

class Buffer
[ New(sender) ▸
    object this : EMPTY·#Insert + FULL(?)·#Remove
    [ EMPTY   & Insert(x, sender) ▸ this!FULL(x) & sender!Reply(this)
    | FULL(x) & Remove(sender)    ▸ this!EMPTY & sender!Reply(x, this)
    ] this!EMPTY & sender!Reply(this)
]

object User : Loop(?)
[ Loop(buffer) ▸ let x, buffer = buffer.Remove in
    	       	 User!Loop(buffer.Insert(x + 1))
]

User!Loop(Buffer.New.Insert(0))
