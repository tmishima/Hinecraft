Hinecraftビルド・インストール手順(windows)
=========

WindowsでHinecraftをビルドするためには、ビルドに使用するPCに以下のプログラムおよびライブラリがインストールされている必要があります。

●Haskell Platform

●FTGL（注１）

●その他依存パッケージ

ビルドに必要な環境が整った後、以下の手順を実行します。



1.	任意のフォルダへHinecraftのクローンを作成します。
2.	コマンドプロンプトまたはPowershellを開き、クローンを作成したフォルダ（Hinecraft.cabalファイルが存在するフォルダ）に移動します。
[以降、Hinecraft.cabalファイルが存在するフォルダを作業フォルダと呼ぶ]
3.	（作業フォルダ）\dll_for_Windows7_32bitフォルダに存在するftgl.dllファイルを作業フォルダへコピーします。
4.	以下のコマンドを実行しプログラムをビルドします。
  A)	 cabal clean
  B)	 cabal configure　　（注3）
  C)	 cabal build
5.	リソースファイル類をインストールします。
  A)	“.Hinecraft”フォルダをユーザのhomeフォルダ（注2）へコピーします。
6.	プログラムを実行します。
  A)	作業フォルダでcabal runを実行します。

* 注１）	HinecraftはFTGL[http://sourceforge.net/apps/mediawiki/ftgl/index.php?title=Main_Page]に依存しています。cabalを使用してFTGLパッケージをインストールする際にインクルードフォルダとlibファイルを指定する必要があります。
インクルードフォルダはFTGLサイトよりソースファイルをダウンロードして指定してください。libファイルは（作業フォルダ）\extra_lib_for_Windows7_32bitフォルダ内のftgl.libを指定してください。

* 注２）	Windows7においては通常以下のフォルダです。
C:\Users\(ログインユーザ名)

* 注３）	依存パッケージがインストールされていない場合は以下のようなエラーが表示されます。
必要な依存パッケージをインストールしてからcabal configureを再実行してください。
cabal: At least the following dependencies are missing:
FTGL -any,
GLFW-b -any,　　　：


