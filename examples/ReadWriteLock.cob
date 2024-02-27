
type #Reader = *Read(Reply(Release))
and  #Writer = *Write(Reply(Release))
in

class ReadWriteLock : *New(Reply(#Reader·#Writer))
[ New(sender) ▸
    object this
      : #Reader·#Writer·(FREE + SHARED·*(READER·Release) + UNIQUE·Release)
    [ FREE   & Read(sender)     ▸ this!SHARED & this!READER & sender!Reply(this)
    | FREE   & Write(sender)    ▸ this!UNIQUE & sender!Reply(this)
    | UNIQUE & Release          ▸ this!FREE
    | SHARED & Read(sender)     ▸ this!SHARED & this!READER & sender!Reply(this)
    | SHARED & READER & Release ▸ this!SHARED
    | SHARED & ¬READER          ▸ this!FREE
    ]
    this!FREE & sender!Reply(this)
]

done
