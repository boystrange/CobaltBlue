// SUBTLE ERROR

// in this case the compiler flags a problem concerning Object, even
// if the problem is really about Main. The point is that Main uses
// Object as it is supposed to to, namely invoking the m
// method. However, Main is used twice in a way which is disallowed by
// its protocol. Therefore, the saturation phase is unable to derive
// constraints about Object/x, whose type remains unbounded.

// probably in this case the compiler should issue an error earlier
// on, during the saturation phase, when it is clear that there is no
// molecule of the smaller type that contains a given molecule of the
// larger type. Even if this is the finite (that is approximate)
// semantics, it is already clear that there is a type mismatch

// note that the problem on Main may or may not manifest depending on
// the order in which lower bounds are checked

// SUBTLE ERROR

object Object : M
[ M ▸ Object!M ]

object Main : A + B(?) + 1
[ A ▸ done
| B(x) ▸ x!M ]

Main!A & Main!B(Object)
