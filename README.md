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
@ seq " " 

### sub
