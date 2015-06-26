###ニューラルネットワーク；クラス分類；多入力変数；未チューニング###
#入力変数が2次元以上だとグラフ化できない

#データの準備
all.data <- iris				#data.frame,全データの書き込み,irisはもともとRに入っているデータ
data <- all.data[,1:4]			#data.frame,解析する項目の選択,今回は数値データの1~4を使用し、4を目的変数とする
n <- nrow(data)				#dataのサンプル数の抽出
ids <- sample(n,n*0.8)			#dataのうち8割をランダムにサンプリングし訓練データとする
tdata <- data[ids,]
pdata <- data[-ids,]			#残りの2割を予測に用いる(テストデータ)

#NNの訓練	*目標変数の設定,初めての際はinstall.packages("nnet")でパッケージをインストールする
library(nnet)
nn <- nnet(Petal.Width~.,tdata,size=10,linout=T)		#未チューニング,linout=Tで回帰

#未知のデータの予想	*直線に乗っているほど実際の値と予想の値が近い
prediction <- predict(nn,pdata)
plot(pdata$Petal.Width,prediction,xlab="pdata",ylab="prediction",xlim=c(0,3),ylim=c(0,3))
x <- 0:3
t <- x
par(new=T)
plot(x,t,type="l",xlab="",ylab="",xlim=c(0,3),ylim=c(0,3))