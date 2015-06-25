#@@@グラフデータから整数計画法の入力形式への変換,最短距離問題の整数計画法を用いた解法@@@

###データ読み込み###
#辺の重み行列w
w <-read.table("spp1.txt")
#頂点数n
n <-nrow(w)
#始点s(要入力)
s <-1
#終点t(要入力)
t <-7
#辺の有無行列e
e <-w
for(i in 1:n){for(j in 1:n){e[i,j] <-if(is.finite(w[i,j])){1}else{0}}}
#辺数ne
ne <-sum(e)

###LP行列m###
#objective function
d <-matrix(0:0,ncol=(n*n+2))
for(i in 1:n){for(j in 1:n){if(e[i,j]==1){d[(n*(i-1)+j)] <-w[i,j]}}}
m <-data.frame(d)
names(m) <-c(1:(n*n),"=<>","y")		   ###ポインタを用いてわかりやすくする
#subjec to
d <-m
for(i in 1:n){
	d[,] <-0
	if(i==s){
		for(j in 1:n){
			if(e[i,j]==1){d[,(n*(i-1)+j)] <-(-1)*e[i,j]}
		}
		d[,n*n+1] <-"="
		d[,n*n+2] <-(-1)
	}else{if(i==t){
		for(j in 1:n){
			if(e[j,i]==1){d[,(n*(j-1)+i)] <-e[j,i]}
		}
		d[,n*n+1] <-"="
		d[,n*n+2] <-1
	}else{
		for(j in 1:n){
			if(e[i,j]==1){d[,(n*(i-1)+j)] <-(-1)*e[i,j]}
			if(e[j,i]==1){d[,(n*(j-1)+i)] <-e[j,i]}
		}
		d[,n*n+1] <-"="
		d[,n*n+2] <-0
	}}
	m <-rbind(m,d)
}
#各変数の非負条件
for(i in 1:n){for(j in 1:n){if(e[i,j]==1){
	d[,] <-0
	d[,(n*(i-1)+j)] <-e[i,j]
	d[n*n+1] <-">="
	d[n*n+2] <-0
	m <-rbind(m,d)
}}}

###整数計画用の様式に変換###
f.obj <-c(m[1,1:(n*n)])
f.con <-m[2:(n+ne+1),1:(n*n)]
f.dir <-m[2:(n+ne+1),(n*n+1)]
f.rhs <-m[2:(n+ne+1),(n*n+2)]
#整数計画問題を解く
library(lpSolve)
r <-matrix(lp("min",f.obj,f.con,f.dir,f.rhs,int.vec=1:(n*n))$solution,n:n,byrow=T)
#最短距離d
d=0
for(i in 1:n){for(j in 1:j){if(r[i,j]==1){d <-d+w[i,j]};d}}

###結果表示###
m
e
r
d



###手入力する場合###
#頂点数n
#n <-5
#辺の重み行列w
#w <-matrix(c(Inf,10,5,Inf,Inf,Inf,Inf,2,Inf,1,Inf,3,Inf,2,Inf,Inf,Inf,Inf,Inf,6,Inf,Inf,Inf,Inf,Inf),n:n,byrow=T)


