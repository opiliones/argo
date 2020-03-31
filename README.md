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

### リダイレクト

### パイプ

### 短絡評価

### 位置パラメータ

### シグナルハンドリング

## 特有の機能
### スプライシング

### 変数

### 関数

### コマンド置換

### マクロ

### スレッド

## 制御構造
### 条件分岐

### 繰り返し

### 局所脱出

### 大域脱出

## 関数
### リスト操作

### ハッシュテーブル

### 文字列操作

### 数値演算

### ファイル操作
