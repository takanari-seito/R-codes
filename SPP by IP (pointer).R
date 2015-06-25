#@@@グラフデータ(ポインタ)から整数計画法の入力形式への変換,最短距離問題の整数計画法を用いた解法@@@

###データフレームw読み込み(辺v->u;重みw)###		Excel上であらかじめソートしておく
data <-read.table("graph3(pointer).txt")　#要変更
names(data) <-c("v","u","w")
#頂点数n
n <-max(data$v,data$u)
#辺数ne
ne <-nrow(data)
#始点s
s <-1			#要変更
#終点
t <-18		#要変更

###LP行列m###
#objective function
d <-matrix(0:0,ncol=(ne+2))
for(i in 1:ne){d[i]　<-data[i,3]}
m <-data.frame(d)
names(m) <-c(1:ne,"=<>","y")
#subject to
d <-m
d[,] <-0
for(i in 1:(n+ne)){m <-rbind(m,d)}
for(i in 1:ne){
	v <-data[i,1]
	u <-data[i,2]
	if(v!=t){m[(v+1),i] <-(-1)}
	if(u!=s){m[(u+1),i] <-1}
	m[(i+n+1),i] <-1
	m[(i+n+1),(ne+1)] <-">="
}
for(i in 1:n){
	v <-data[i,1]
	m[(i+1),(ne+1)] <-"="
	m[(i+1),(ne+2)] <-if(i==s){-1}else{if(i==t){1}else{0}}
}

###整数計画用の様式に変換###
f.obj <-c(m[1,1:ne])
f.con <-m[2:(n+ne+1),1:ne]
f.dir <-m[2:(n+ne+1),(ne+1)]
f.rhs <-m[2:(n+ne+1),(ne+2)]
#整数計画問題を解く
library(lpSolve)
result <-lp("min",f.obj,f.con,f.dir,f.rhs,int.vec=1:ne)$solution	#"min"->"max"で最長距離問題に変更可能(回路を含まない場合)
data <-cbind(data,result)
#最短距離d
d=0
for(i in 1:ne){d <-d+data[i,3]*data[i,4]}
###ポインタの行列表示###
wm <-matrix(nrow=n,ncol=n)
wm[,] <-Inf
for(i in 1:ne){wm[(data[i,1]),(data[i,2])] <-data[i,3]}
em <-matrix(nrow=n,ncol=n)
em[,] <-0
for(i in 1:ne){if(data[i,4]==1){em[(data[i,1]),(data[i,2])] <-1}}
#道順path
path <-s
v <-s
while(v!=t){
	i <-1
	while(em[v,i]!=1){
		i=i+1
	}
	v <-i
	path <-paste(path,v)
}


###結果表示###
m
data
wm
em
d
path




