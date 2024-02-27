// minimal version of the Pi example

class Worker
[ New(id, sender) ▸
    object this : READY(?)·Start(?) + WORKING(?,?) + 1
    [ READY(id) & Start(master) ▸
	System.Print("Worker " ^ Value.ToString(id) ^ " starts");
        this!WORKING(id, master)
    | WORKING(id, master) ▸
        System.Wait(Number.Random(10));
	master!Done(id)
    ] this!READY(id) & sender!Reply(this)
]

class Master
[ New(sender) ▸
    object this
      : READY·Calculate(?) +
      	CREATE_WORKERS(?)·CLIENT(?)·*(WORKER·Done(?)) +
      	WAIT_WORKERS·CLIENT(?)·*(WORKER·Done(?)) +
      	NOTIFY·CLIENT(?) + 1
    [ READY & Calculate(client) ▸
        this!CLIENT(client) &
	this!CREATE_WORKERS(Number.Random(10))
    | CREATE_WORKERS(n_workers) ▸
        if n_workers > 0 then
	  Worker.New(n_workers)!Start(this) &
	  this!WORKER &
	  this!CREATE_WORKERS(n_workers - 1)
	else
	  this!WAIT_WORKERS
    | WAIT_WORKERS & WORKER & Done(id) ▸
	System.Print("Worker " ^ Value.ToString(id) ^ " done");
        this!WAIT_WORKERS
    | WAIT_WORKERS & ¬WORKER ▸ this!NOTIFY
    | NOTIFY & CLIENT(client) ▸ client!Reply
    ] this!READY & sender!Reply(this)
]

Master.New.Calculate;
done