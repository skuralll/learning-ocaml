(* 与えられた正整数 n に対し (12+22+⋯+n2) を計算する再帰関数 squaresum : int -> int を定義せよ *)
let rec squaresum n = if n = 1 then 1 else n * n + squaresum(n - 1);;
let x = squaresum(3);;
print_int(x);;
print_endline "";;

(* 与えられた正整数 n に対し，n番目のフィボナッチ数を計算する再帰関数 fib : int -> int を定義せよ． *)
let rec fib n = if n <= 2 then 1 else fib(n-1) + fib(n-2);;
let y = fib(4);;
print_int(y);;
print_endline "";;

(* ユークリッドの互除法を使ってふたつの正整数 n, mの最大公約数を計算する再帰関数 gcd : int * int -> int を定義せよ *)
let rec gcd(n, m) = if n < m then gcd(m,n) else let n' = n mod m in if n' = 0 then m else gcd(m, n');;
let z = gcd(8177, 3315);;
print_int(z);;