SICP 読書ノート#10 - SICPのためのプログラミング環境構築
======================================
「§2.2.4 図形言語」に入る前にグラフィックスまわりの環境設定が必要だったので、
遅ればせながらScheme処理系であるRacketやEmacsの設定も含めてまとめてみました。

- **(2015/04/29追記) 内容が古くなってきたので全体的に加筆/修正しました。**
- **(2015/12/02追記) DrRacketの使い方などを追加しました**

PC環境
--------------------------------
- Mac OS X Yosemite 10.10.2
- Homebrew 0.9.5
- GNU Emacs 24.4.1

HomebrewやEmacsの環境構築は以下を参照ください。

[http://uents.hatenablog.com/entry/2015/01/19/OSXYosemite%E3%81%ABHomebrew%E3%82%92%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB:embed]

[http://uents.hatenablog.com/entry/2015/01/26/OSXYosemite%E3%81%ABHomebrew%E3%81%A7GNUEmacs%E3%82%92%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB:embed]


Scheme処理系
--------------------------------
[MITの推奨はRacket（旧PLT-Scheme/DrScheme）](http://cl.naist.jp/index.php?SICP%CA%D9%B6%AF%B2%F1)らしいです。

Schemeと[Racket](https://ja.wikipedia.org/wiki/Racket)は言語仕様で所々違いはあるのですが、
[SICPのサンプルコード](https://mitpress.mit.edu/sicp/code/index.html)は
おおよそRacketの処理系で動かせますし、ライブラリが豊富なので使い勝手はよいと思います。

僕の場合はHomebrew Caskで取得しました。

```sh
% brew cask install racket
```

DrRacketをつかう
--------------------------------
Homebrew CaskでRacketをインストールするとDrRacketというIDEがついてきます。

- Launchpadやターミナルなどから `~/Applications/DrRacket.app` を起動します
- 画面下部のREPLでSchemeのコードを直接入力し実行します
- または [ファイル]→[開く...] からSchemeのコードを書いたファイルを開き [実行] をクリックします

![drracket](https://farm6.staticflickr.com/5821/23449575065_6a02150458_o_d.png)

ファイルで開く場合は、そのファイルの先頭に
```scheme
#lang racket
```
のようにシェバンを書く必要があります。

また、[実行] の替わりに [デバッグ] をクリックすると、ブレイクポイントを設定したり
ステップ実行を行うこともできます。§4の処理系のように抽象度の高いプログラムを追う時に帳票します。


EmacsでSchemeを動かす
--------------------------------
DrRacketはデバッガもついていて便利なのですが、
複数のファイルに分割しながら実装するなどがやりにくいため、
僕は普段はEmacsで実装を行っています。

それにはEmacsでRacketを動かすためのmajor/minor-modeとしてGeiserを導入すると便利です。

### Geiserのインストール
Emacs Caskで取得できます。Caskファイルに以下を追加し、

```elisp
(source gnu)
(source melpa)
(depends-on "geiser")
```

ターミナルでインストールを実行。

```
% cask install
```

### .emacs.elの設定
geiserの設定を追加します。

```elisp
(setq geiser-racket-binary "/opt/homebrew-cask/Caskroom/racket/6.1.1/Racket v6.1.1/bin/racket")
(setq geiser-active-implementations '(racket))
(setq geiser-repl-read-only-prompt-p nil) ;; Racket REPL上で(read)で入力を取る時に必要
```

### Racket REPLを起動
Emacsを開き```M-x run-racket```とするとRacketが起動します。

![emacs-geiseir](https://farm6.staticflickr.com/5717/23367054501_702e257df6_o_d.png)

REPL上でSchemeのコードを直接書いて行ってもよいですし、
ファイルを読み込みたい場合は```,enter "foo.scm"``` とすればよいです。

[http://nongnu.org/geiser/geiser_3.html#Switching-context:embed]

また、

- 拡張子が`.scm`の場合、major-modeはScheme、minor-modeはRacketとなるようです
- Auto Complete Modeを事前に導入していると、minor-modeにAuto Completeが追加されます


#### デバッグ

##### プリント出力
```scheme
racket@> (define x 1)
racket@> (define y 2)
racket@> (display (format "~a ~a ~%" x y))
```

#### 手続きをトレース
- http://docs.racket-lang.org/reference/debugging.html

```scheme
racket@> (require racket/trace)
racket@> (trace <procedure name>)
```

その他
--------------------------------

### Auto Complete Modeについて
以前に書いた記事が参考になるかもしれません。(かなり古いです)

[http://uents.hatenablog.com/entry/20120311/1331468314:embed]

Emacs Caskを使っての設定方法に書き換えないと…

--------------------------------

※「SICP読書ノート」の目次は[こちら](/entry/sicp/index)
