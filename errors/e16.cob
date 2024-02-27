
// this class is ill-typed because of unbounded type variables. It is
// not known how "this" returned to New will be used, hence the tool
// forces a 1 upper-bound which however is invalid according to the
// type A·M. By specifing the type New(reply(M)) for C the tool is
// able to conclude that the class is well typed.

class C
[ New(sender) ▸
    object this : A·M [ ]
    this!A & sender!Reply(this) ]

done