###ニューラルネットワーク；クラス分類；2入力変数；グラフ化；未チューニング###
#グラフ化する際には入力変数として1変数しか用いることができないことに注意

#データの準備
all.data <- iris				#data.frame,全データの書き込み,irisはもともとRに入っているデータ
tdata <- all.data[,c(1,3)]		#data.frame,解析する項目の選択,入力変数とする1列目,目標変数とする3列目を選択

#NNの訓練	*目標変数の設定,初めての際はinstall.packages("nnet")でパッケージをインストールする
library(nnet)
nn <- nnet(Petal.Length~.,tdata,size=10,linout=T)		#未チューニング,linout=Tで回帰

###以下グラフ化###
#等高線グラフの準備(特定の範囲の点を大量に作り、nnに代入してプロットする)
l <- 1000									#軸の分割数(1000くらいから遅くなる)
input1 <- seq(4,8,length=l)
input2 <- expand.grid(input1)						#data.frame,範囲は訓練データのみのプロットの範囲を参考に
names(input2) <- "Sepal.Length"					#nnは列の名前を参考に入力変数を探している?
output <- predict(nn,input2)						#data.frame,predict(nn,x=data.frame)
plot(input1,output,xlab="Sepal.Length",ylab="Petal.Length",xlim=c(4,9),ylim=c(0,8),type="l")
										#type="l"で点をつなぐ

#訓練データのプロット
x <- data[,1]
t <- data[,2]
par(new=T)
plot(x,t,xlab="",ylab="",xlim=c(4,9),ylim=c(0,8))


#訓練データのみ,範囲の設定の参考に
x <- tdata[,1]
t <- tdata[,2]
plot(x,t)