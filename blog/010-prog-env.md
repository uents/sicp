計算機科学の勉強にSICPを読もう - #10 Racket/Emacsによるプログラミング環境構築
======================================

「§2.2.4 図形言語」に入る前にグラフィックスまわりの環境設定が必要だったので、
遅ればせながらScheme処理系であるRacketやEmacsの設定も含めてまとめてみた。

PC環境
--------------------------------

Mac OS X 10.7.5 (Lion) です。

- Homebrew
```
$ brew --version
0.9.5
```

- Emacs (Homebrewでインストール済み)
```
% emacs --version
GNU Emacs 24.3.1
```

Scheme処理系
--------------------------------

[最近のMITの推奨はRacket（旧PLT-Scheme/DrScheme）](http://cl.naist.jp/index.php?SICP%CA%D9%B6%AF%B2%F1)らしい。

Homebrewで取得しようとするが、

```
 brew install plt-racket
==> Downloading https://github.com/plt/racket/archive/v6.0.1.tar.gz
Already downloaded: /Users/uents/Library/Caches/Homebrew/plt-racket-6.0.1.tar.gz
==> ./configure --enable-macprefix --prefix=/usr/local/Cellar/plt-racket/6.0.1 --man=/usr/local/Cellar/plt-r
==> make
make[4]: *** [xsrc/precomp.h] Segmentation fault: 11
make[3]: *** [all] Error 2
make[2]: *** [3m] Error 2
make[1]: *** [3m] Error 2
make: *** [all] Error 2

READ THIS: https://github.com/Homebrew/homebrew/wiki/troubleshooting

These open issues may also help:
plt-racket formula perhaps too minimal (https://github.com/Homebrew/homebrew/issues/29914)
```

ビルドの途中でSIGSEGVで落ちる。

よくわからないので、

1. 公式サイト(http://download.racket-lang.org/)からdmgを取得
2. dmgをオープンしてApplicationフォルダにコピー

で、インストール完了。


Emacsの設定
--------------------------------

Racketのエディタが使いづらかったので、Emacsをセットアップ。

EmacsでScheme処理系を動かすためのmajor/minor-modeとしてGeiserを導入した。

### Geiserのインストール

EmacsでELPAの設定まで済んでいる前提で、```M-x package-list-packages```し、geiserを選択。

### .emacs.elの設定

Racketへのパスを通す。

```elisp
(setq geiser-racket-binary "/Applications/Racket6.0.1/bin/racket")
(setq geiser-active-implementations '(racket))
```

### Racket REPLを起動

- 拡張子が.scmの場合、major-modeはScheme、minor-modeはRacketとなる
- Auto Complete Modeを事前に導入していると、minor-modeにAuto Completeが追加される

この状態で```M-x run-racket```とすると、Racketが起動する。

![RacketをEmacsで起動](https://farm3.staticflickr.com/2929/14423624370_8f74e98a57_o_d.png)

後は、Schemeのコードを書いていけば良いし、
ファイルを読み込みたい場合は```(load "foo.scm")``` とすればOK。

### デバッグ

#### print文

```scheme
(define x 1)
(define y 2)
(define z 3)

(display (format "~a ~a ~a ~%" x y z))
```

#### 手続きをトレース

```scheme
(require racket/trace)
(trace <procedure name>)
```

とすればよい。

http://docs.racket-lang.org/reference/debugging.html


その他
--------------------------------

### HomebrewやEmacsの導入

以前に書いた記事が参考になるかもしれません。(少し古いです)

- [MacでHomebrewを使ってCocoa Emacsを導入する](/entry/20120303/1330745761)
- [EmacsでAnything＋Auto Complete Mode＋YASnippetによる快適コーディング](/entry/20120311/1331468314)
 + ELPAに触れてないので、そろそろ書き直さないと...

--------------------------------

※「計算機科学の勉強にSICPを読もう」の目次は[こちら](/entry/sicp/index.md)
