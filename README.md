# argo

Common Lisp(SBCL)で作られたsh風の言語です。
マクロを使ってlispのコードに変換してから実行するのでスクリプトが高速に動作します。
(フォークは普通に遅いです。処理系の起動時間も。)

posix互換ではありません。

## オプション
argo.lisp \[-b BINARY-FILE] \[-c STRING|FILE] \[-x]
argo \[-b BINARY-FILE] \[-c STRING|FILE]

* bオプション: cオプション引数又はFILEのコードをコンパイルしてBINARY-FILEを作成します。
* cオプション: STRINGを実行します。
* xオプション: argo自身のバイナリを作成します。

引数を指定しない場合はREPLが起動します。

## コード例

### フィボナッチ数列

```
fn fib {
  lt $1 3 && return 1
  fib {$1`-`1}`+ fib {$1`-`2}
}

fib {num $1} -> echo
```

### Fizz Bazz

```
seq 1 100 | loop {
  read || return -> ^{
    let n (num $1)
    ^{
       mod $n 15`= 0 =>: FizzBuzz
       mod $n 3 `= 0 =>: Fizz
       mod $n 5 `= 0 =>: Buzz
       : $n
    } -> echo
  }
}
```
又は

```
seq 1 100 | loop {
  read || return -> ^{
    let n {num $1}
    echo (COND
      ({mod $n 15`= 0} FizzBuzz)
      ({mod $n 3 `= 0} Fizz)
      ({mod $n 5 `= 0} Buzz)
      ($T $1))
  }
}
```

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
@ let stat errno {ls -1}; echo $stat $errno
a
b
c
T 0
@ let stat errno {/bin/false}; echo $stat $errno
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
@ {echo a; echo b > 2} > 2 1 > c; cat < c
a
b
```

### パイプ

デフォルトでは戻り値は一番右のコマンドのものと同一です。
\*PIPE-POLICY\*で制御できます。値は以下のとおりです。

|値|意味|
|--|---|
|last|一番右のコマンド|
|left|すべての第一戻り値が非nilの場合、一番右のコマンド。そうでない場合は第一戻り値がnilのもののうち一番左のコマンドの戻り値。|
|right|すべての第一戻り値が非nilの場合、一番右のコマンド。そうでない場合は第一戻り値がnilのもののうち一番右のコマンドの戻り値。|
|cons|各コマンドの戻り値(list)をconsで結合する。|

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
@ ^{echo $2} a b c
b
@ ^{echo $*} a b c
(a b c)
@ ^{echo @$*} a b c
a b c
@ ^{shift; echo $2} a b c
c
```

## 特有の機能
### スプライシング

```
@ echo :{a b c}
(a b c)
@ echo @:{a b c}
a b c
```

なお、`:{...}`は`{list ...}`の省記です。

### パイプ演算子

```
@ /bin/true -> echo
T
@ /bin/true --> echo
T 0
```

### 変数

```
@ var x 1
@ echo $x
1
@ let x :{1 2 3}; cdr $x -> echo
(2 3)
@ const y 1
@ echo $y
1
@ let y 2; echo $y
;; -> コンパイルエラーになる
```

### 関数

```
@ fn f {echo hello $1}
@ f world
hello  world
```

### コマンド置換

```
@ echo {echo a | read-all}
a
```

### マクロ

```
@ mac m `{echo $1 $$1; echo @$1 @$$1}
@ ^{m :{1 2 3}} :{a b c}
(a b c) (1 2 3)
a b c 1 2 3
```

### スレッド

&でコマンドを接続した場合、並列実行し、両方のコマンドが終わるまで待ち合わせます。
戻り値はパイプと同様に\*PARA-POLICY\*で制御できます。

```
@ let x {{sleep 1} & sleep 2}; echo $x
T
```

### 中置演算子

任意の演算子について、\`をつけるとコマンド同士を接続する中置演算子、
\`で囲むと値を引数に取る中置演算子だと認識します。

```
@ true `when echo a
a
```

```
@ 1`+`1 -> echo
2
```

## 制御構造
だいたいCLのやり方と同じ。

### 条件分岐

```
@ if $T {echo a} {echo b}
a
@ if $NIL {echo a} {echo b}
b
@ cond ({eq a b} {echo a}) ({eq b b} {echo b})
b
@ case 1 (0 {echo 0}) (1 {echo 1}) (2 {echo 2})
1
```

`()`で囲むと内部を以下のルールでパースします。

- リストの最初の項を関数又は値だとみなす
- 改行を無視する

condやcaseといったlispのマクロに渡す場合、引数はネイティブなlistである必要があります。

### 繰り返し

```
@ loop {echo yes}
yes
yes
yes
...
```
```
@ loop for i in :{1 2 3} collect $i -> echo
(1 2 3)
```

### 局所脱出

```
@ block a {echo a; return-from a 0; echo b}
a
```

### 大域脱出

```
@ fn f {echo a; throw a 1; echo b}
@ catch a {f} -> echo
a
1
```

## 独自コマンド

### ulist

```
@ ulist :{1 2 3} --> echo
1 2 3
```

### dict / idx / modf / rem / udict

```
@ dict a 1 b 2 -> idx a -> echo
1
@ let x {dict a 1 b 2}; modf $x b 4 c 3 -> udict -> echo
(a 1 b 4 c 3)
@ dict a 1 b 2 -> rem a -> udict -> echo
(b 2)
```
### tmpf
一時的なストリームを与えられた関数の第一引数に与える。

```
@ tmpf ^{echo a > $1; cat < $1}
a
```
### glob

```
@ glob * -> echo
(a b c)
```
### form

```
@ echo {form "~16,8f~%" $PI}
      3.14159265
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
@ sep " "  " a  b c 12  " -> echo
( a  b c 12)
@ sep " "  " a  b c 12  " -> usep , -> echo
,a,,b,c,12 
```

### sub

```
@ sub o 0 "Hello World" -> echo
Hell0 W0rld
```

### num

```
@ + {num "1"} {num 1} -> echo
2
```

### その他

```
@ ^{echo $1; = 1 $1 && return; echo a} 1
1
@ values 1 2 --> echo
1 2
@ let a 1; eval `{let a 2; echo $a $$a}
2 1
@ format $T '~a~%' hello
hello
@ block $NIL {unwind-protect {return; echo b} {echo a}}
a
@ cons 1 2 -> echo
(1 . 2)
@ listp :{1 2} -> echo
T
@ var a :{1 2 3}
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
@ mapcar ^{echo $1; + 1 1} $a -> echo $a
1
2
3
(2 3 4)
@ remove-if ^{= 1 $1} $a -> echo
(2 3)
@ remove-if-not ^{= 1 $1} $a -> echo
(1)
@ reduce + $a -> echo
6
@ remove-duplicates :{1 1 2 3} -> echo
(1 2 3)
@ reverse $a -> echo
(3 2 1)
@ append $a {reverse $a} -> echo
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
@ and {eql 1 1} a -> echo
a
@ or {eql "a" "a"} a -> echo
a
@ when {eql 1 1} {echo a} {echo b}
a
b
@ unless {eql "a" "a"} {echo a} {echo b}
a
b
@ numberp 1 -> echo
T
@ max 1 3 2 -> echo
3
@ min 2 1 3 -> echo
1
@ sin {/ $PI 2} -> echo
1.0d0
@ cos $PI -> echo
-1.0d0
@ tan {/ $PI 4} -> echo
1.0d0
@ exp 2 -> echo
7.389056
@ expt 2 2 -> echo
4
@ sqrt 16 -> echo
4.0
@ log 1 -> echo
0.0
@ abs -1 -> echo
1
@ conjugate {complex 1 1} -> echo
#C(1 -1)
@ phase {complex 1 1} -> echo
0.7853982
@ signum -2.0 -> echo
-1.0
@ cis {/ $PI 4} -> echo
#C(0.7071067811865476d0 0.7071067811865475d0)
@ gcd 144 120 -> echo
24
@ lcm 144 120 -> echo
720
@ floor $PI -> echo
3
@ ceiling $PI -> echo
4
@ round $PI -> echo
3
@ ffloor $PI -> echo
3.0d0
@ fceiling $PI -> echo
4.0d0
@ fround $PI -> echo
3.0d0
@ truncate {* -1 $PI} -> echo
-3
@ ftruncate {* -1 $PI} -> echo
-3.0d0
@ mod $PI 1 -> echo
0.14159265358979312d0
@ rem {* -1 $PI} 1 -> echo
-0.14159265358979312d0
@ float 1 -> echo
1.0
@ rational 0.99999999 -> echo
1
@ numerator {/ 3 12} -> echo
1
@ denominator {/ 3 12} -> echo
4
@ realpart {complex 1 1} -> echo
1
@ imagpart {complex 1 1} -> echo
1
@ zerop 0 -> echo
T
@ plusp 1 -> echo
T
@ minusp -1 -> echo
T
@ evenp 1 -> echo
NIL
@ oddp 1 -> echo
T
@ integerp 1 -> echo
T
@ floatp 1.0 -> echo
T
@ rationalp 1 -> echo
T
@ realp 1 -> echo
T
@ complexp 1 -> echo
NIL
@ subseq "hello" 3 -> echo
lo
@ let a "Zeppo Marx"; replace $a "Harpo" :end1 5 -> echo
@ concatenate STRING "hello" " " "world" -> echo
hello world
@ map STRING ^{echo $1; : $1} "hello"
h
e
l
l
o
@ string-upcase "hello" -> echo
HELLO
@ string-downcase "HELLO" -> echo
hello
@ string-capitalize "hello world" -> echo
Hello World
@ string-trim " " "  hello  " -> echo
hello
@ string a -> echo
a
@ intern "a" -> echo
a
@ search "we" "If we can't be free we can at least be cheap" :from-end $T -> echo
20
@ parse-integer "42" :radix 8 -> echo
34
@ read-from-string '#\a' -> echo
a
@ string= "a" "a" -> echo
T
@ string-equal "a" "A" -> echo
T
@ string-gt "a" "b" -> echo
NIL
@ string-lt "a" "b" -> echo
0
@ scan "b" "abc" -> echo
1
@ scan-to-strings "\\d+" "Originally specified in 1958, Lisp is" -> echo
1958
@ all-matches-as-strings "\\w+" "foo bar baz" -> echo
(foo bar baz)
@ regex-replace "<" "<<<" "&lt;" -> echo
&lt;<<
@ regex-replace-all "<" "<<<" "&lt;" -> echo
&lt;&lt;&lt;
@ split "," "a,b,c" -> echo
(a b c)
```
