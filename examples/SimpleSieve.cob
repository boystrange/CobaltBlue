type #Get = Get(Reply(#Number, #Get)) in

// se non si specifica il tipo del secondo argomento di New c'e` un
// consumo spropositato di memoria, forse e` un bug

object source : STATE(?)·#Get
[ STATE(n) & Get(target) ▸ source!STATE(n + 1) & target!Reply(n, source) ]
source!STATE(2) &

object printer : Run(?)
[ Run(source) ▸
    object filter : READY(?,?)·#Get + WAIT(?,?,?)
    [ READY(k, source) & Get(target) ▸ filter!WAIT(k, source, target)
    | WAIT(k, source, target) ▸
        let n, source = source.Get in
	if n % k = 0 then
	  filter!WAIT(k, source, target)
	else
	  filter!READY(k, source) & target!Reply(n, filter)
    ]
    let n, source = source.Get in
    filter!READY(n, source) &
    System.Print(n); printer!Run(filter)
]

printer!Run(source) &

System.Wait(60); done