計算機科学の勉強にSICPを読もう - #10 Racket/Emacsによるプログラミング環境構築
======================================

「§2.2.4 図形言語」に入る前に、Scheme処理系およびグラフィックスまわりの環境設定メモ

PC環境
--------------------------------

Mac OS X 10.7.5 (Lion) です。

- Homebrew
```
$ brew --version
0.9.5
```

- Emacs
 + Homebrewでインストール済み
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

1. 公式サイト(http://download.racket-lang.org/)からdmgファイルを取得
2. dmgファイルをオープンしてApplicationフォルダにコピー

で、インストール完了。


Emacsの設定
--------------------------------

Racketのエディタが使いにくいので、Emacsをセットアップ。

EmacsでScheme処理系を動かすためのmajor/minor-modeとしてGeiserを導入した。

### Geiseirのインストール

EmacsでELPAの設定まで済んでいる前提で、```M-x package-list-packages```し、geiserを選択。

### .emacs.elの設定

Racketへのパスを通す。

```elisp
(setq geiser-racket-binary "/Applications/Racket6.0.1/bin/racket")
(setq geiser-active-implementations '(racket))
```

### Emacsを起動

- 拡張子が.scmの場合、major-modeはScheme、minor-modeはRacketとなる
- Auto Complete Modeを事前に導入していると、minor-modeにAuto Completeが追加される

この状態で```M-x run-racket```とすると、Racketが起動する。

後は、Schemeのコードを書いていけば良いし、
ファイルを読み込みたい場合は```(load "foo.scm")``` とすればOK。


直線や画像の描画
--------------------------------

- http://www.neilvandyke.org/racket-sicp/

によると、RacketのSICPプラグインを読み込めば、paintという手続きで画像が表示できるが、
直線などのプリミティブな描画についてはよくわからない。

ググったところ Racket Graphics Legacy Library にたどり着く。

- http://docs.racket-lang.org/graphics/index.html
- http://stackoverflow.com/questions/13592352/compiling-sicp-picture-exercises-in-drracket

Exampleなどを参考にすれば、だいたい使い方はわかる。


### キャンバスのオープン

白いキャンバスウィンドウが表示される。

```scheme
(require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))
```

### 直線の描画

```scheme
((draw-line vp) (make-posn 100 400) (make-posn 400 100))
```

### 画像の描画

```scheme
((draw-pixmap vp) "lenna.png" (make-posn 50 50))
```

### キャンバスをクリア

```scheme
((clear-viewport vp))
```

### キャンバスのクローズ

```scheme
(close-viewport vp)
(close-graphics)
```


その他
--------------------------------

HomebrewやEmacsの導入は以前に書いた記事が参考になるかもしれません。(少し古いです)

- [MacでHomebrewを使ってCocoa Emacsを導入する](http://uents.hatenablog.com/entry/20120303/1330745761)
- [EmacsでAnything＋Auto Complete Mode＋YASnippetによる快適コーディング](http://uents.hatenablog.com/entry/20120311/1331468314)
 + ELPAを使ってないな。そろそろ書き直さないと...


--------------------------------

※「計算機科学の勉強にSICPを読もう」の目次は[こちら](http://uents.hatenablog.com/entry/2014/05/25/000000)
