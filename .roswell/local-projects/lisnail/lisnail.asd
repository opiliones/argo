;;; lisnail.asd
(in-package :cl-user)    ; どのパッケージにいるかわからないのでCL-USERパッケージにする
(defpackage :lisnail-asd  ; ASDFのシステム定義用のパッケージをつくる
  (:use :cl :asdf))      ; 標準関数とASDFの関数をパッケージ修飾なしで呼べるようにする
(in-package :lisnail-asd) ; 作ったパッケージにする

(defsystem :lisnail
  :class :package-inferred-system   ; システム定義のスタイルをpackage-inferred-systemにする
  :description "Programing language like sh embedded in lisp"
  :version "0.1"
  :author "opiliones"
  :license "BSD 3"
  :depends-on ("lisnail/main"))  ; ソースファイルを相対パスで指定する
