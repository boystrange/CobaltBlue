// Lock with non-blocking acquire

class Lock
[ New(sender) ▸
    object this : *TryAcquire(?)·(FREE + BUSY·Release)
    [ FREE & TryAcquire(sender) ▸ this!BUSY & sender!True(this)
    | BUSY & TryAcquire(sender) ▸ this!BUSY & sender!False
    | BUSY & Release            ▸ this!FREE ]
    this!FREE & sender!Reply(this)
]

case Lock.New.TryAcquire of
[ True(lock) ▸ System.Print("lock acquired!");
  	       lock!Release
| False      ▸ done ]

