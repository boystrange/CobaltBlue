// Lock class, with many users

type #Acquire = *Acquire(Reply(#Release))
and  #Release = Release
in

class Lock : *New(Reply(#Acquire))
[ New(sender) ▸
    object this : #Acquire·(FREE + BUSY·#Release)
    [ FREE & Acquire(sender) ▸ this!BUSY & sender!Reply(this)
    | BUSY & Release         ▸ this!FREE ]
    this!FREE & sender!Reply(this)
]

class Philosopher : *New(#String, #Acquire)
[ New(name, fork) ▸
    object this
      : FORK(#Acquire)·(THINKING(#String) + HUNGRY(#String) + EATING(#String, ?))
    [ THINKING(name) ▸
        let t = Number.Random(7) in
      	System.Print(name ^ " thinks for "
			  ^ Value.ToString(t)
			  ^ " second(s)...");
     	System.Wait(t);
      	this!HUNGRY(name)
    | FORK(fork) & HUNGRY(name) ▸
        System.Print(name ^ " is hungry...");
	{ this!FORK(fork) & this!EATING(name, fork.Acquire) }
    | EATING(name, fork) ▸
        System.Print(name ^ " eats...");
	System.Wait(1);
	{ fork!Release & this!THINKING(name) }
    ]
    this!FORK(fork) & this!THINKING(name)
]

let fork = Lock.New in
Philosopher!New("Socrate", fork) &
Philosopher!New("Platone", fork) &
Philosopher!New("Parmenide", fork) &
Philosopher!New("Empedocle", fork) &
Philosopher!New("Anassimene", fork) &
System.Wait(60);
done
