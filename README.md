Hinecraft
=========

This is a simple Minecraft Clone (DEMO) written in Haskell.

暇だったのでHaskell(OpenGL,GLFW,etc)でMinecraftっぽいものを作ってみました。まだ全然完成してないけど。  
データのRead/Write部分が最適化されていないので、起動時にハングアップしているように見えます。

![ScreenShot](https://raw.github.com/tmishima/Hinecraft/master/Hinecraft_title.png)
![ScreenShot](https://raw.github.com/tmishima/Hinecraft/master/Hinecraft_blocks.png)

* 注意

1. Hinecraftの開発はUbuntu 13.10上で行っています。
2. ghc ver.7.6.3 cabal 1.18.0.2を使用してビルドしています。
3. 現バージョン(Ver0.2.x.x)は各種リソース(Texture,Font)のパスが固定されています。
4. 実行する際には以下の手順が必要です。  
  a. ".Hinecraft" ディレクトリ(Texture,Font関連)をHomeディレクトリへコピーする。  

Twitter：　@tty_mishima  

テクスチャは以下をお借りしてます。  

* TEX-D2  
http://forum.minecraftuser.jp/viewtopic.php?f=14&t=3132  
（タイトルの一部を変更しています）  

* あずき
解像度：16*16px  
対応バージョン：1.7.x  
配布ページ：http://hashibami.nobody.jp/  
作者名：ueda  


##デモアプリのビルド手順(windowsの場合)

###1、source treeを起動してビルドするフォルダーをダウンロードする。

  
  1. 新規クローンを作成する
  
  2. 元のパスに<https://github.com/tmishima/Hinecraft.git>を指定する
  
  3. 保存先のパスにそれ専用のフォルダーを作成して、それを指定する。


###2、コマンドでビルドを始める。

　1. コマンドを起動して保存パスに指定したディレクトリーへ移動する。

　2. `cabal clean`を実行

　3. `cabal configure`を実行する

　　3-1. エラーメッセージが出た場合(注釈１に記す)
      

　4. `cabal build`を実行する。実行が成功すると、distフォルダーに実行ファイルが出来る。

　5. 実行ファイルを保存パスのフォルダーに移動。


  注釈１
  
    Config file path source is default config file.
  
    Config file C:\Users\k10001kk\AppData\Roaming\cabal\config not found.

    Writing default configuration to

    C:\Users\k10001kk\AppData\Roaming\cabal\config

    Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal

    update' to download it.

    Resolving dependencies...

    Configuring Hinecraft-0.2.0.0...

    cabal: At least the following dependencies are missing:
  
    FTGL -any,

    GLFW-b -any,

    GLUtil -any,

    JuicyPixels -any,

    cereal -any,

    linear –any





