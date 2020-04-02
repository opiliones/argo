# argo

Common Lisp(SBCL)で作られたsh風の言語です。
マクロを使ってlispのコードに変換してから実行するのでスクリプトが高速に動作します。
(フォークは普通に遅いです。処理系の起動時間も。)

できるだけCLの文法に寄せているのでposix互換ではありません。

## オプション
argo \[-b BINARY-FILE] \[-c STRING|FILE]

* bオプション: cオプション引数又はFILEのコードをコンパイルしてBINARY-FILEを作成します。
* cオプション: STRINGを実行します。

引数を指定しない場合はREPLが起動します。

## shに似た機能
### コマンド実行

```
@ echo hello world !
hello world !
@ touch a b c
@ ls -1
a
b
c
```

外部コマンドの戻り値はbool値です。
エラーコードは２番目の戻り値です。

```
@ let stat errno ls -1; echo $stat $errno
a
b
c
T 0
@ let stat errno /bin/false; echo $stat $errno
NIL 1
```

### リダイレクト

```
@ echo a > a; cat < a
a
@ echo b > 2 b
b
@ cat b
@ echo a >> 1 a; cat < a
a
a
@ (echo a; echo b > 2) > c > 2 1; cat < c
a
b
```

### パイプ

```
@ echo a | grep a | grep b
@ yes | head -n 1
yes
```

### 短絡評価

```
@ true && echo a
a
@ false && echo a
@ true || echo a
@ false || echo a
a
@ true && echo a || echo b
a
@ false && echo a || echo b
b
@ true && false || echo b
b
@ false || false || echo a || echo b
a
```

### 位置パラメータ

```
@ ^(echo $2) a b c
b
@ ^(echo $*) a b c
(a b c)
@ ^(echo @$*) a b c
a b c
@ ^(shift; echo $2) a b c
c
```

## 特有の機能
### スプライシング

```
@ echo @(list a b c)
a b c
```

### パイプ演算子

```
@ /bin/: -> echo
T
@ /bin/: --> echo
T 0
```

### 変数

```
@ let x (list 1 2 3); cdr $x
(2 3)
```

### 関数

```
@ fn f (echo hello $1)
@ f world
hello  world
```

### コマンド置換

```
@ echo (echo a | read)
a
```

### マクロ

```
@ mac m {echo $1 $$1; echo @$1 @$$1}
@ ^(m (list 1 2 3)) (list a b c)
(a b c)
(1 2 3)
a b c
1 2 3
```

### スレッド

&でコマンドを接続した場合、並列実行し、両方のコマンドが終わるまで待ち合わせる。

```
@ let x ((sleep 1; : 1) & : 2)
@ echo x
(1 . 2)
```

## 制御構造
だいたいCLのやり方と同じ。

### 条件分岐

```
@ if true (echo a) (echo b)
a
@ if false (echo a) (echo b)
b
@ cond ((eq a b) (echo a)) ((eq b b) (echo b))
b
@ case 1 (0 (echo 0)) (1 (echo 1)) (2 (echo 2))
1
```

### 繰り返し

```
@ loop (echo yes) | head -n 3
yes
yes
yes
@ loop for i in (list 1 2 3) collect $i -> echo
(1 2 3)
```

### 局所脱出

```
@ block a (echo a; return-from a 0; echo b)
a
```

### 大域脱出

```
@ fn f (echo a; throw a; echo b)
@ catch a (f)
a
```

## 独自コマンド

### ulist

```
@ ulist (list 1 2 3) --> echo
1 2 3
```

### dict / idx / modf / udict

```
@ dict a 1 b 2 -> idx a -> echo
1
@ let a (dict a 1 b 2); modf x b 4 c 3 -> udict -> echo
(a 1 b 4 c 3)
```
### tmpf

```
@ tmpf ^(echo a > $1; cat $1)
a
```
### glob

```
@ glob * -> echo
(a b c)
```
### form

```
@ echo (form '~~%' $PI)

```

### true / false / :

```
@ true 1 --> echo
T 1
@ false 1 --> echo
NIL 1
@ : 1 --> echo
1
```

### sep / usep

```
@ sep " a  b c 12  " -> echo
(a b c 12)
@ seq " "  " a  b c 12  " -> echo
( a  b c 12)
@ seq " "  " a  b c 12  " -> usep , -> echo
,a,,b,c,12 
```

### sub

```
@ sub o 0 "Hello World" -> echo
Hell0 W0rld
```

### その他

```
@ ^(echo $1; = 1 $1 && return; echo a) 1
1
@ values 1 2 --> echo
1 2
@ let a 1; eval {let a 2; echo $a $$a}
2 1
@ format $T '~a~%' hello
hello
@ block $NIL (unwind-protect (return; echo b) (echo a))
a
@ cons 1 2 -> echo
(1 . 2)
@ listp (list 1 2) -> echo
T
@ var a (list 1 2 3)
@ car $a -> echo
1
@ rest $a -> echo
(2 3)
@ init $a -> echo
(1 2)
@ first $a -> echo
1
@ second $a -> echo
2
@ third $a -> echo
3
@ fourth $a -> echo
NIL
@ nth 0 $a -> echo
1
@ last $a -> echo
(3)
@ length $a -> echo
3
@ mapcar ^(echo $1; + 1 1) $a -> echo $a
1
2
3
(2 3 4)
@ remove-if ^(= 1 $1) $a -> echo
(2 3)
@ remove-if-not ^(= 1 $1) $a -> echo
(1)
@ reduce + $a -> echo
6
@ remove-duplicates (list 1 1 2 3) -> echo
(1 2 3)
@ reverse $a -> echo
(3 2 1)
@ append $a (reverse $a) -> echo
1 2 3 3 2 1
@ eq a a -> echo
T
@ eq $a $a -> echo
NIL
@ eql 1 1 -> echo
T
@ eql "a" "a" -> echo
NIL
@ equal $a $a -> echo
T
@ and (eql 1 1) a -> echo
a
@ or (eql "a" "a") a -> echo
a
@ when (eql 1 1) (echo a) (echo b)
a
b
@ unless (eql "a" "a") (echo a) (echo b)
a
b
@ numberp 1 -> echo
T
@ max 1 3 2 -> echo
3
@ min 2 1 3 -> echo
1
@ sin (/ $PI 2) -> echo
1.0d0
@ cos $PI -> echo
-1.0d0
@ tan (/ $PI 4) -> echo
1.0d0
@ exp expt sqrt log
  sqrt isqrt
  abs
  conjugate phase signum cis
  gcd lcm
  floor ceiling round ffloor fceiling fround
  truncate ftruncate mod rem
  float
  rational rationalize
  numerator denominator
  complex realpart imagpart
  zerop plusp minusp evenp oddp integerp floatp rationalp realp complexp
  subseq replace concatenate map
  string-upcase string-downcase string-capitalize string-trim
  string intern find-if position-if count-if position count search
  parse-integer read-from-string
  string= string-equal string< string< string-lessp
  scan scan-to-strings all-matches-as-strings regex-replace regex-replace-all split
  make-hash-table gethash remhash
```
