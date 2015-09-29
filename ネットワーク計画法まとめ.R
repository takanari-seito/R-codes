####ネットワーク計画問題まとめ####
install.packages("igraph")
library(igraph)

#main関数
#点数を入力
nv = 10
data = makeRWN(nv)
shortestPath(data,"1","2")


#点数nvのランダム重み付きネットワークの作成
makeRWN = function(nv){
	data = random.graph.game(nv,0.5,directed=T)
	ne = ecount(data)
	w = w=floor(runif(ne,min=0,max=100))
	E(data)$weight = w
	print(get.data.frame(data))
	plot(data,edge.label=E(data)$weight)
	return(data)
}

#最短経路
shortestPath = function(data,s,t){
	#path有無の確認と不可能の時の表示
	sum = get.all.shortest.paths(data,s,t,mode="out")
	#重みの合計を求めて表示
	print(sum$res)
	V(data)[sum$res[[1]]]
	E(data)$color = "grey"
	E(data,path=sum$res[[1]])$color <- "red"
	E(data,path=sum$res[[1]])$width <- 3
	plot(data,vertex.size=8,edge.label=E(data)$weight)
}

#ハミルトン閉路

#最短巡回路

#最少全域木

#最大クリーク

#頂点被覆

#最大流














#http://d.hatena.ne.jp/ryamada/20111227/1324799553
#http://d.hatena.ne.jp/Rion778/20100329/1269794943
#http://d.hatena.ne.jp/Rion778/20100405/1270474673
#http://kappa-bioinformatics.blogspot.jp/2013/01/blog-post_19.html
###http://deta.hateblo.jp/entry/2013/05/01/053426
#https://sites.google.com/site/kztakemoto/r-seminar-on-igraph---supplementary-information