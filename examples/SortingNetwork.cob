
class Aux : *ShowArray(#Array, Reply(#String))
[ ShowArray(a, sender) ▸
    sender!Reply("[" ^ Value.ToString(Array.Get(a, 0)) ^ ","
    	       	     ^ Value.ToString(Array.Get(a, 1)) ^ ","
	       	     ^ Value.ToString(Array.Get(a, 2)) ^ ","
	       	     ^ Value.ToString(Array.Get(a, 3)) ^ ","
	       	     ^ Value.ToString(Array.Get(a, 4)) ^ ","
	       	     ^ Value.ToString(Array.Get(a, 5)) ^ "]")
]

class Comparator
[ New(up, down, sender) ▸
    object this : EMPTY(?,?)·Value(#Number)·Value(#Number) +
      	   	  ONE(?,?,?)·Value(?) + 1
    [ EMPTY(up, down) & Value(x) ▸ this!ONE(up, down, x)
    | ONE(up, down, x) & Value(y) ▸
        if x ≤ y then
	  up!Value(x) & down!Value(y)
	else
	  up!Value(y) & down!Value(x)
    ] this!EMPTY(up, down) & sender!Reply(this)
]

class Collector
[ New(a, i, c, sender) ▸
    object this : EMPTY(?,?,?)·Value(?) + 1
    [ EMPTY(a, i, c) & Value(x) ▸ Array.Set(a, i, x); c!Done ]
    this!EMPTY(a, i, c) & sender!Reply(this)
]

class SortingNetwork
[ Phase(o₁, o₂, o₃, o₄, o₅, o₆, sender) ▸
    let c₄ = Comparator.New(o₂, o₃) in
    let c₅ = Comparator.New(o₄, o₅) in
    let c₁ = Comparator.New(o₁, c₄) in
    let c₂ = Comparator.New(c₄, c₅) in
    let c₃ = Comparator.New(c₅, o₆) in
    sender!Reply(c₁, c₁, c₂, c₂, c₃, c₃)
| Phases(o₁, o₂, o₃, o₄, o₅, o₆, sender) ▸
    let p₃₁, p₃₂, p₃₃, p₃₄, p₃₅, p₃₆ = SortingNetwork.Phase(o₁, o₂, o₃, o₄, o₅, o₆) in
    let p₂₁, p₂₂, p₂₃, p₂₄, p₂₅, p₂₆ = SortingNetwork.Phase(p₃₁, p₃₂, p₃₃, p₃₄, p₃₅, p₃₆) in
    SortingNetwork!Phase(p₂₁, p₂₂, p₂₃, p₂₄, p₂₅, p₂₆, sender)
| New(v₁, v₂, v₃, v₄, v₅, v₆, sender) ▸
    object this : WAIT(?)·*(COLLECTOR·Done) + 1
    [ WAIT(sender) & COLLECTOR  & Done ▸ this!WAIT(sender)
    | WAIT(sender) & ¬COLLECTOR        ▸ sender!Reply ]
    let a = Array.New(6, 0) in
    Array.Set(a, 0, v₁);
    Array.Set(a, 1, v₂);
    Array.Set(a, 2, v₃);
    Array.Set(a, 3, v₄);
    Array.Set(a, 4, v₅);
    Array.Set(a, 5, v₆);
    System.Print("BEFORE: " ^ Aux.ShowArray(a));
    this!COLLECTOR & this!COLLECTOR & this!COLLECTOR &
    this!COLLECTOR & this!COLLECTOR & this!COLLECTOR &
    let c₁ = Collector.New(a, 0, this) in
    let c₂ = Collector.New(a, 1, this) in
    let c₃ = Collector.New(a, 2, this) in
    let c₄ = Collector.New(a, 3, this) in
    let c₅ = Collector.New(a, 4, this) in
    let c₆ = Collector.New(a, 5, this) in
    let i₁, i₂, i₃, i₄, i₅, i₆ = SortingNetwork.Phases(c₁, c₂, c₃, c₄, c₅, c₆) in
    i₁!Value(v₁) & i₂!Value(v₂) & i₃!Value(v₃) &
    i₄!Value(v₄) & i₅!Value(v₅) & i₆!Value(v₆) &
    this.WAIT;
    System.Print("AFTER: " ^ Aux.ShowArray(a));
    sender!Reply
]

SortingNetwork.New(Number.Random(100),
		   Number.Random(100),
		   Number.Random(100),
		   Number.Random(100),
		   Number.Random(100),
		   Number.Random(100));
done
