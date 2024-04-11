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
print_endline "";;

(* 単純なヴァリアント *)
type furikake = Shake | Katsuo | Nori;;
let isVeggie f =
  match f with
    Shake -> false
  | Katsuo -> false
  | Nori -> true
;;
Printf.printf "Shake: %B\n" (isVeggie(Shake));
Printf.printf "Nori: %B\n" (isVeggie(Nori));

(* 少し複雑なヴァリアント *)
type miso = Aka | Shiro | Awase;;
type gu = Wakame | Tofu | Radish;;
type dish = PorkCutlet | Soup of {m: miso; g: gu} | Rice of furikake;; (* とんかつ | 味噌汁 | ご飯 *)
let food = Soup{m=Aka;g=Tofu};;
let isSolid d = 
  match d with
      PorkCutlet -> true
    | Soup m_and_g -> false
    | Rice f -> true
;;
Printf.printf "Soup: %B\n" (isSolid(Soup{m=Aka;g=Tofu}));;
(* 付帯情報を使う *)
let price_of_dish d = 
    match d with
      PorkCutlet -> 350
    | Soup m_and_g -> 90
    | Rice (Shake | Katsuo) -> 90 
    | Rice Nori -> 80
  ;;
  Printf.printf "Shake: %d\n" (price_of_dish(Rice(Shake)));;
(* 一品が菜食主義者でも食べられるかどうかを判定する関数 isVeggieDish : dish -> bool を定義せよ． *)
let isVeggieDish d = 
  match d with
    PorkCutlet -> false
  | Soup m_and_g -> true
  | Rice f -> isVeggie f
;;
Printf.printf "PorkCutlet: %B\n" (isVeggieDish(PorkCutlet));;
Printf.printf "Rice(Nori): %B\n" (isVeggieDish(Rice(Nori)));;
(* 再帰ヴァリアント *)
type menu = Smile | Add of {d: dish; next: menu};;
let m1 : menu = Smile;;
let m2 : menu = Add {d=PorkCutlet;next=m1};;
let m3 : menu = Add {d=Rice Nori;next=m2};;
let m4 : menu = Add {d=Rice Shake;next=m3};;
let rec price_of_menu m =
  match m with
    Smile -> 0
  | Add {d=d1;next=m'} -> price_of_dish d1 + price_of_menu m'
;;
Printf.printf "Menu: %d\n" (price_of_menu m4);;
(* 定食が菜食主義者でも食べられるかどうかを判定する再帰関数 isVeggieMenu : menu -> bool を定義せよ． *)
let rec isVeggieMenu m =
  match m with
    Smile -> true
  | Add {d=d1;next=m'} -> isVeggieDish d1 && isVeggieMenu m'
;;
Printf.printf "Menu: %B\n" (isVeggieMenu m4);;
