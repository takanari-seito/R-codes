###ニューラルネットワーク；クラス分類；2入力変数；グラフ化；未チューニング###
#グラフ化する際には入力変数として1,2変数しか用いることができないことに注意,出力は設定したクラスの確率

#データの準備
all.data <- iris				#data.frame,全データの書き込み,irisはもともとRに入っているデータ
data <- all.data[,c(1,3,5)]		#data.frame,解析する項目の選択,入力変数とする1,3列目,目標変数とする5列目を選択
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

###以下グラフ化###
#等高線グラフの準備(特定の範囲の点を大量に作り、nnに代入してプロットする)
l <- 100					#軸の分割数(1000くらいから遅くなる)
input1 <- seq(4,9,length=l)						#vector,範囲は訓練データのみのプロットの範囲を参考に
input2 <- seq(0,8,length=l)						#vector
input <- expand.grid(input1,input2)					#data.frame
names(input) <- c("Sepal.Length","Petal.Length")		#nnは列の名前を参考に入力変数を探している?
output <- predict(nn,input)[,1]					#data.frame,出力するクラスを指定
output <- matrix(output,l:l)						#matrix
cols <- colorRampPalette(c("white","red"))			#色の設定
image(input1,input2,output,xlab="Sepal.Length",ylab="Petal.Length",
	xlim=c(4,9),ylim=c(0,8),col=cols(5))			#image(x1=vector,x2=vector,t=matrix)????

#訓練データのプロット
data1 <- subset(tdata,Species=="setosa")				#data.frame,それぞれの品種のみを含むデータの作成
data2 <- subset(tdata,Species=="versicolor")
data3 <- subset(tdata,Species=="virginica")
x11 <- data1[,1];x12 <- data1[,2]					#data.frame,それぞれの入力変数の抽出
x21 <- data2[,1];x22 <- data2[,2]
x31 <- data3[,1];x32 <- data3[,2]
par(new=T)	#グラフを重ねる
plot(x11,x12,xlim=c(4,9),ylim=c(0,8),xlab="",ylab="",pch=1)
par(new=T)
plot(x21,x22,xlim=c(4,9),ylim=c(0,8),xlab="",ylab="",pch=20)
par(new=T)
plot(x31,x32,xlim=c(4,9),ylim=c(0,8),xlab="",ylab="",pch=4)





#訓練データのみ,範囲の設定の参考に
x1 <- data[,1]
x2 <- data[,2]
plot(x1,x2)