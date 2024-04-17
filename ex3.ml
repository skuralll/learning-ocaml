(* 既存のレコードを使って新しいレコードを作った場合(functional update)でも既存のレコードは変化しない *)
type point = {x: int; y:int};;
let origin : point = {x=5; y=5;};;
let p = {origin with y= 3};;
(* 変更可能レコード *)
type mutable_point = {mutable x: int; mutable y: int;};;
let m_origin = {x=0; y=0;};;
m_origin.x <- 2;;
Printf.printf "%d\n" m_origin.x;;
(* 以下のような式を実行したときに起こる、値がもとまる以外のこと(この場合は1と画面に表示されるということ)を計算効果(computational effect)という． *)
Printf.printf "%d\n" 1;;
let p1 : mutable_point = {x=0;y=0;};;
let p2 = p1;; (* p1にp2という別名をつけているという解釈　参照をコピーしている? *)
p2.x <- 3;;
Printf.printf "p1.x = %d\n" p1.x;; (* 参照しているレコードは同じ為、p1.xから得られる値も変わっている *)
(* ref型 *)
(* 省略 *)
(* 逐次実行 *)
p1.x <- 0; p2.y <- 4; Printf.printf "%d\n" p1.y; 100;;
(* 条件分岐 *)
let is_positive n = 
  if n > 0 then print_string "n is positive\n"
  else print_string "n is not positive\n";;
is_positive 100;;
is_positive (-100);;
let is_positive2 n = 
  if n > 0 then
    begin 
      print_int n;
      print_string " is positive\n"
    end
  else
    begin
      print_int n;
      print_string " is not positive\n"
    end;;
is_positive2 100;;
is_positive2 (-100);;
let is_positive3 n = 
  if n > 0 then
    ( 
      print_int n;
      print_string " is positive\n"
    )
  else
    (
      print_int n;
      print_string " is not positive\n"
    );;
is_positive3 100;;
is_positive3 (-100);;
(* ループ *)
for i = 1 to 10 do print_int i; print_newline() done;;