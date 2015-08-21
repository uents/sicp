SICP 読書ノート#10 - Racket/Emacsによるプログラミング環境構築
======================================

「§2.2.4 図形言語」に入る前にグラフィックスまわりの環境設定が必要だったので、遅ればせながらScheme処理系であるRacketやEmacsの設定も含めてまとめてみた。

*(2015/04/29追記) 内容が古くなってきたので全体的に加筆/修正しました。*

PC環境
--------------------------------

- Mac OS X Yosemite 10.10.2
- Homebrew 0.9.5
- GNU Emacs 24.4.1

ここまでの環境構築は以下を参照ください。

- [/entry/2015/01/19/OSXYosemiteにHomebrewをインストール:title]
- [/entry/2015/01/26/OSXYosemiteにHomebrewでGNUEmacsをインストール:title]


Scheme処理系
--------------------------------

[最近のMITの推奨はRacket（旧PLT-Scheme/DrScheme）](http://cl.naist.jp/index.php?SICP%CA%D9%B6%AF%B2%F1)らしい。

Homebrew Caskで取得しました。

```sh
% brew cask install racket
==> Downloading http://mirror.racket-lang.org/installers/6.1.1/racket-6.1.1-x86_64-macosx.dmg
...
==> Symlinking App Suite 'Racket v6.1.1' to '/Users/uents/Applications/Racket v6.1.1'
==> Symlinking Binary 'drracket' to '/usr/local/bin/drracket'
==> Symlinking Binary 'gracket' to '/usr/local/bin/gracket'
==> Symlinking Binary 'gracket-text' to '/usr/local/bin/gracket-text'
==> Symlinking Binary 'mred' to '/usr/local/bin/mred'
==> Symlinking Binary 'mred-text' to '/usr/local/bin/mred-text'
==> Symlinking Binary 'mzc' to '/usr/local/bin/mzc'
==> Symlinking Binary 'mzpp' to '/usr/local/bin/mzpp'
==> Symlinking Binary 'mzscheme' to '/usr/local/bin/mzscheme'
==> Symlinking Binary 'mztext' to '/usr/local/bin/mztext'
==> Symlinking Binary 'pdf-slatex' to '/usr/local/bin/pdf-slatex'
==> Symlinking Binary 'plt-games' to '/usr/local/bin/plt-games'
==> Symlinking Binary 'plt-help' to '/usr/local/bin/plt-help'
==> Symlinking Binary 'plt-r5rs' to '/usr/local/bin/plt-r5rs'
==> Symlinking Binary 'plt-r6rs' to '/usr/local/bin/plt-r6rs'
==> Symlinking Binary 'plt-web-server' to '/usr/local/bin/plt-web-server'
==> Symlinking Binary 'racket' to '/usr/local/bin/racket'
==> Symlinking Binary 'raco' to '/usr/local/bin/raco'
==> Symlinking Binary 'scribble' to '/usr/local/bin/scribble'
==> Symlinking Binary 'setup-plt' to '/usr/local/bin/setup-plt'
==> Symlinking Binary 'slatex' to '/usr/local/bin/slatex'
==> Symlinking Binary 'slideshow' to '/usr/local/bin/slideshow'
==> Symlinking Binary 'swindle' to '/usr/local/bin/swindle'
🍺  racket staged at '/opt/homebrew-cask/Caskroom/racket/6.1.1' (22059 files, 448M)
```

Emacsの設定
--------------------------------

RacketをインストールするとDrRacketという結構本格的なIDEが付属しています。

僕にはどうも合わなかったので、EmacsでRacketを動かすためのmajor/minor-modeとしてGeiserを導入した。

### Geiserのインストール

Emacs Caskで取得できます。Caskファイルに以下を追加し、

```elisp
(source gnu)
(source melpa)

(depends-on "geiser")
```

ターミナルでインストールを実行。

```sh
% cask install
```

### .emacs.elの設定

geiserの設定を追加します。

```elisp
(setq geiser-racket-binary "/opt/homebrew-cask/Caskroom/racket/6.1.1/Racket v6.1.1/bin/racket")
(setq geiser-active-implementations '(racket))
```

### Racket REPLを起動

Emacsを開き```M-x run-racket```とすると、Racketが起動する。

![RacketをEmacsで起動](https://farm3.staticflickr.com/2929/14423624370_8f74e98a57_o_d.png)

あとはSchemeのコードを書いていけば良いし、ファイルを読み込みたい場合は```(load-relative "foo.scm")``` とすればよいです。

また、

- 拡張子が`.scm`の場合、major-modeはScheme、minor-modeはRacketとなる
- Auto Complete Modeを事前に導入していると、minor-modeにAuto Completeが追加される


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

http://docs.racket-lang.org/reference/debugging.html


その他
--------------------------------

### Auto Complete Modeについて

以前に書いた記事が参考になるかもしれません。(かなり古いです)

- [EmacsでAnything＋Auto Complete Mode＋YASnippetによる快適コーディング](/entry/20120311/1331468314)
 + Emacs Caskを使った場合に書き直さないと...

--------------------------------

※「SICP読書ノート」の目次は[こちら](/entry/sicp/index)
