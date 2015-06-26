###ニューラルネットワーク；クラス分類；多入力変数；未チューニング###
#入力変数が3次以上なのでグラフ化はできない

#データの準備
all.data <- iris				#data.frame,全データの書き込み,irisはもともとRに入っているデータ
data <- all.data				#data.frame,解析する項目の選択,今回は全項目を用いる
n <- nrow(data)				#dataのサンプル数の抽出
ids <- sample(n,n*0.8)			#dataのうち8割をランダムにサンプリングし訓練データとする
tdata <- data[ids,]
pdata <- data[-ids,]			#残りの2割を予測に用いる(テストデータ)

#NNの訓練	*目標変数の設定,初めての際はinstall.packages("nnet")でパッケージをインストールする
library(nnet)
nn <- nnet(Species~.,tdata,size=10)		#未チューニング

#未分類のデータの予測
prediction <- predict(nn,pdata,type="class")	#typeを入力しないと確率が出力される
table(prediction,pdata$Species)
