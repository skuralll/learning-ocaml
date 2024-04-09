type point = {x: int; y: int;};;
(* ふたつの点を引数として，その二点を直径とする円の面積を返す関数 area : point * point -> float を定義せよ
整数 n は float_of_int という関数を呼ぶことで float に変換することができる．平方根は sqrt 関数で計算できる． *)
let area(p1, p2) = let square x = x * x in
                   let radius = sqrt(float_of_int(square(p1.x - p2.x) + square(p1.y - p2.y))) /. 2.0 in
                   radius *. radius *. 3.14;;
let point1 = {x=0;y=0};;
let point2 = {x=2;y=0};;
let result = area(point1, point2);;
print_float(result);;
print_endline "";;

(* ふたつの有理数の和を求める関数 sum : rational * rational -> rational を定義せよ． *)
type rational = {
  num : int; (* 分子 *)
  den : int; (* 分母 *)
};;

let rec gcd(n, m) =
  if n < m then gcd(m, n)
  else
    let n' = n mod m in
    if n' = 0 then m else gcd(m, n')

let sum(r1, r2) = let r3_num = r1.num * r2.den + r2.num * r1.den in
                  let r3_den = r1.den * r2.den in
                  let g = gcd(r3_num, r3_den) in
                  {num=r3_num/g; den=r3_den/g;};;
let rational1 = {num=1; den=2};;
let rational2 = {num=3; den=4};;
let sum_result = sum(rational1, rational2);;
print_int(sum_result.num);;
print_string("/");;
print_int(sum_result.den);;