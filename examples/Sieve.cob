type #Source = Get(Reply(#Number, #Source))
in

// se non si specifica il tipo del secondo argomento di New c'e` un
// consumo spropositato di memoria, forse e` un bug

class Source : *New(#Number, Reply(#Source))
[ New(n, r) ▸
    object this : STATE(?)·Get(?)
    [ STATE(n) & Get(target) ▸ this!STATE(n + 1) & target!Reply(n, this) ]
    this!STATE(n) & r!Reply(this)
]

class Filter
[ New(k, source, r) ▸
    object this : READY(?,?)·Get(?) + WAIT(?,?,?)
    [ READY(k, source) & Get(target) ▸ this!WAIT(k, source, target)
    | WAIT(k, source, target) ▸
        let n, source = source.Get in
	if n % k = 0 then
	  this!WAIT(k, source, target)
	else
	  this!READY(k, source) & target!Reply(n, this)
    ] this!READY(k, source) & r!Reply(this)
]

class Printer
[ New(source) ▸
    object this : Run(?)
    [ Run(source) ▸
        let n, source = source.Get in
	let filter = Filter.New(n, source) in
        System.Print(n);
	this!Run(filter)
    ] this!Run(source)
]

Printer!New(Source.New(2)) &

System.Wait(60); done