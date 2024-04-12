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
