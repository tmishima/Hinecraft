Hinecraft
=========

This is a simple Minecraft Clone (DEMO) written in Haskell.

暇だったのでHaskell(OpenGL,GLFW,etc)でMinecraftっぽいものを作ってみました。まだ全然完成してないけど。  
データの保存機能もなくα版にも満たない状況です。  

![ScreenShot](https://github.com/tmishima/Hinecraft/blob/master/Hinecraft_title.png "ScreenShot")

* 注意

1. Hinecraftの開発はUbuntu 13.10上で行っています。
2. ghc ver.7.6.3 cabal 1.18.0.2を使用してビルドしています。
3. 現バージョン(Ver0.0.0.3)は各種リソース(Texture,Font)のパスが固定されています。
4. ビルド・実行する際には以下の手順が必要です。  
  a. ".Hinecraft" ディレクトリ(Texture関連)をHomeディレクトリへコピーする。  
  b. 実行環境に合わせてHinecraft/Render/View.hs loadGuiResource 関数のfontPathを設定する。  
 
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
