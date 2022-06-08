# 競技プログラミング

## 置いてあるもの

- これまでに解答した問題のソースコード
- AtCoder用の補助ツール
- ライブラリ (整備中)

## ソースコード

- `atcoder`: AtCoder の Algorithm コンテスト
- `ahc`: AtCoder の Heuristic コンテスト
- `aoj`: Aizu Online Judge
- `poj`: Peking University Online Judge

の各ディレクトリに，各オンラインジャッジサービスで提出したコードを保存しています．

## AtCoder用の補助ツール

`tools/` にAtcoderコンテスト用の補助ツール(`atctools`)を置いています

- `atctools gen <contest-name>`: 指定したコンテストの問題に対する入出力例をダウンロード
- `atctools test <task-id> <program>`: `task-id`で指定した問題について，自分のプログラム(`program`)が入出力例を満たすかチェック

## ライブラリ

現在は，デバッグ補助用の`debug`のみ．
アルゴリズム等のライブラリも書いたら置く予定

- `debug.cpp`: デバッグ補助
  - `std::pair`やSTLコンテナ(`std::vector`等)用の出力オペレータオーバーロード
  - 可変引数のpython風`print()`関数
