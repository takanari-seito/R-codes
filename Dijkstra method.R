#最短距離問題のダイクストラ法による解法
#input	:n*nの距離行列
#output	:スタート点sからの各点への最短距離(label;NAは到達不可能)とパス(path)

#n*nの距離行列をエクセルから取得する
data=read.table("spp1.txt");

#nを距離行列のサイズ(点数)nrow(data)に設定する
n=nrow(data);

rownames(data)=c(1:n);
colnames(data)=c(1:n);

#各点に対応するサイズnのラベルベクトルlabelを定義する(要素はすべてNA)
label=matrix(ncol=n);
colnames(label)=c(1:n);
for(i in 1:n){label[i]=NA};

#各点に対応するサイズnのパズベクトルpathを定義する(要素は各点)
path=matrix(ncol=n);
colnames(path)=c(1:n);
for(i in 1:n){path[i]=i};

#スタート点sの設定、スタート点sのラベルベクトルlabel[s]を0に設定する
s=1;
label[s]=0;

x=0;
#x(最短のパスの)がInfになるまで続ける(ラベル可能なすべての点がラベル付けされるまで)
while(is.finite(x)){
	x=Inf;y=Inf;z=Inf;a=0;b=0;l=0;
	for(i in 1:n){
		#ラベルのついているある点iと
		if((!is.na(label[i]))){
			for(j in 1:n){
				#点iと結合していて,かつラベルのついていないすべての点jに対して
				if((is.finite(data[i,j]))&&(is.na(label[j]))){
					#点iのラベルlabel[i]に点iから点jへの距離data[i,j]を足し
					y=label[i]+data[i,j];
					#最小値を求める
					z=min(x,y);
					#最小値となった組み合わせに対して点iと点a,点jを点bに更新する
					if(x>y){a=i;b=j};
					x=z;
				};	
			};	
		};	
	};
	#点bのラベルlabel[b]をその最小値の値とする
	label[b]=x;
	#点bへのパスpath[b]に点aへのパスpath[a]を左から加える
	path[b]=paste(path[a],path[b]);
};

cat("距離行列");cat("\n");
print(data);
cat("スタート点");cat(s);cat("からの各点への最短距離");cat("\n");
print(label);
cat("パス");cat("\n");
print(path);