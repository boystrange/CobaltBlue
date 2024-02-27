// recursive factorial and fibonacci

class Math
[ Fact(n, sender) ▸
    if n ≤ 0 then sender!Reply(1)
    else sender!Reply(n × Math.Fact(n - 1))
| Fibo(n, sender) ▸
    if n ≤ 1 then sender!Reply(n)
    else sender!Reply(Math.Fibo(n - 1) + Math.Fibo(n - 2))
]

System.Print(Math.Fact(10));
System.Print(Math.Fibo(10));
done
