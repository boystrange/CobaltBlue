// approximation of Pi, from the Akka tutorial

type #WorkerM = Start(#Number, #Number, #MasterW) + Quit
and  #MasterW = Done(#Number, #Number, #WorkerM)
and  #MasterC = Pi(#Number, #Number, #Number, Reply(#Number))
in

class Worker : *New(#Number, Reply(#WorkerM))
[ New(id, sender) ▸
    object this
      : READY·ID(#Number)·#WorkerM +
        WORKING(#Number, #Number, #Number)·ID(#Number)·MASTER(#MasterW) +
        NOTIFY(#Number)·ID(#Number)·MASTER(#MasterW) + 1
    [ READY & ID(n) & Start(start, n_elements, master) ▸
	this!MASTER(master) &
	let end = start + n_elements in
	System.Print("Worker " ^ Value.ToString(n)
			       ^ " on range "
			       ^ Value.ToString(start)
			       ^ ".."
			       ^ Value.ToString(end));
	this!ID(n) & this!WORKING(0.0, start, end)
    | READY & ID(n) & Quit ▸ done
    | WORKING(acc, i, n) ▸
        if i < n then
	  this!WORKING(acc + (4.0 × (1 - (i % 2) × 2)) / (2 × i + 1), i + 1, n)
    	else
      	  this!NOTIFY(acc)
    | NOTIFY(acc) & ID(n) & MASTER(master) ▸
	this!READY & this!ID(n) & master!Done(n, acc, this) ]

    this!READY & this!ID(id) & sender!Reply(this) ]

class Master : *New(Reply(#MasterC))
[ New(sender) ▸
    object this
      : READY·Pi(?,?,?,?) +
        CREATE_WORKERS(?,?,?)·CLIENT(?)·*IDLE_WORKER(?) +
        CREATE_JOBS(?,?,?)·CLIENT(?)·*IDLE_WORKER(?)·*JOB(?,?) +
	(ASSIGN_JOBS ·CLIENT(?)·*IDLE_WORKER(?) ·*JOB(?,?) +
         WAIT_WORKERS·CLIENT(?)·*IDLE_WORKER(?))·RESULT(?)·*(BUSY_WORKER·Done(?,?,?)) +
        NOTIFY·CLIENT(?)·RESULT(?) + 1
    [ READY & Pi(n_workers, n_jobs, n_elements, client) ▸
        this!CLIENT(client) &
	this!CREATE_WORKERS(n_workers, n_jobs, n_elements)
    | CREATE_WORKERS(n_workers, n_jobs, n_elements) ▸
        if n_workers > 0 then
      	  this!IDLE_WORKER(Worker.New(n_workers)) &
      	  this!CREATE_WORKERS(n_workers - 1, n_jobs, n_elements)
      	else
      	  this!CREATE_JOBS(0, n_jobs, n_elements)
    | CREATE_JOBS(i, n_jobs, n_elements) ▸
      	if i < n_jobs then
      	  this!JOB(i × n_elements, n_elements) &
      	  this!CREATE_JOBS(i + 1, n_jobs, n_elements)
      	else
      	  this!RESULT(0.0) &
      	  this!ASSIGN_JOBS
    | ASSIGN_JOBS & JOB(start, n_elements) & IDLE_WORKER(worker) ▸
        worker!Start(start, n_elements, this) &
      	this!BUSY_WORKER &
	this!ASSIGN_JOBS
    | BUSY_WORKER & Done(id, partial, worker) & RESULT(acc) ▸
      	System.Print("Worker " ^ Value.ToString(id)
			       ^ " has finished: "
			       ^ Value.ToString(partial));
        this!IDLE_WORKER(worker) &
      	this!RESULT(acc + partial)
    | ASSIGN_JOBS & ¬JOB ▸ this!WAIT_WORKERS
    | WAIT_WORKERS & IDLE_WORKER(worker) ▸
        worker!Quit & this!WAIT_WORKERS
    | WAIT_WORKERS & ¬BUSY_WORKER & ¬IDLE_WORKER ▸ this!NOTIFY
    | NOTIFY & CLIENT(client) & RESULT(acc) ▸ client!Reply(acc) ]

    this!READY & sender!Reply(this)
]

System.Print(Master.New.Pi(10, 100, 1000));
done
