
type #ClosedFile = Open(#String, Reply(#OpenFile)) + 1
and  #OpenFile   = More(False(#OpenFile) + True(#ReadyFile)) +
     		   Close(Reply(#ClosedFile))
and  #ReadyFile  = More(True(#ReadyFile)) +
     		   Close(Reply(#ClosedFile)) +
		   Read(Reply(#String, #OpenFile))
in

class File : *New(Reply(#ClosedFile))
[ New(sender) ▸
    object this
      : CLOSED·#ClosedFile +
      	OPEN(#Handle)·#OpenFile +
	READY(#Handle)·#ReadyFile
    [ CLOSED & Open(name, sender) ▸
        this!OPEN(Handle.Open(name)) & sender!Reply(this)
    | OPEN(handle) & More(sender) ▸
	if Handle.EndOfFile(handle) then
	  this!OPEN(handle) & sender!False(this)
	else
	  this!READY(handle) & sender!True(this)
    | OPEN(handle) & Close(sender) ▸
        Handle.Close(handle); { this!CLOSED & sender!Reply(this) }
    | READY(handle) & More(sender) ▸
        this!READY(handle) & sender!True(this)
    | READY(handle) & Read(sender) ▸
        this!OPEN(handle) & sender!Reply(Handle.Read(handle), this)
    | READY(handle) & Close(sender) ▸
        Handle.Close(handle); { this!CLOSED & sender!Reply(this) }
    ]

    this!CLOSED & sender!Reply(this)
]

class Main
[ Pad(s, n, sender) ▸
    if n ≤ 0 then sender!Reply(s)
    else Main!Pad(" " ^ s, n - 1, sender)
| Format(n, sender) ▸
    let s = Value.ToString(n) in
    Main!Pad(s, 4 - String.Length(s), sender)
| Start(sender) ▸
    let file = File.New in
    let file = file.Open("examples/File.cob") in
    Main!Count(0, 0, file, sender)
| Count(lines, chars, file, sender) ▸
    case file.More of
    [ False(file) ▸ Main!Finish(lines, chars, file, sender)
    | True(file)  ▸ let line, file = file.Read in
      		    System.Print(Main.Format(lines + 1) ^ " " ^ line);
      		    Main!Count(lines + 1, chars + String.Length(line), file, sender) ]
| Finish(lines, chars, file, sender) ▸
    let file = file.Close in
    System.Print("# lines = " ^ Value.ToString(lines));
    System.Print("# chars = " ^ Value.ToString(chars));
    sender!Reply
]

Main.Start;
done
