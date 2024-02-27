
class Lock [ New(sender) ▸
  object this : *Acquire(?)·(FREE + BUSY·Release)
  [ FREE & Acquire(sender) ▸ this!BUSY & sender!Reply(this)
  | BUSY & Release         ▸ this!FREE ]

  this!FREE & sender!Reply(this)
]

let lock = Lock.New.Acquire in
System.Print("lock acquired!");
lock!Release & lock!Release // TYPE ERROR //
