#最長距離問題のポテンシャルアップ法による解法
#input	:n*nの距離行列(ループが存在しない)
#output	:スタート点sから各点への最長距離(label;NAは到達不可能)とパス(path)


#####スタート点を変えることができていない#####


#n*nの距離行列をエクセルから取得する
data=read.table("lpp1.txt");

#nを距離行列のサイズ(点数)nrow(data)に設定する
n=nrow(data);

rownames(data)=c(1:n);
colnames(data)=c(1:n);

#各点に対応するサイズnのラベルベクトルlabelを定義する(要素はすべて0)
label=matrix(ncol=n);
colnames(label)=c(1:n);
for(i in 1:n){label[i]=0};

#各点に対応するサイズnのパスベクトルを定義する(要素は各点)
path=matrix(ncol=n);
colnames(path)=c(1:n);
for(i in 1:n){path[i]=i};

#スタート点sを設定
s=1;

###ループがないことの確認を追加したほうがよい###

repeat{
	#処理前のラベルベクトル
	x=label
	#特定の点iと
	for(i in 1:n){
		for(j in 1:n){
			#点iと結合している点jに対して
			if(is.finite(data[i,j])){
				#点iのラベルlabel[i]と点iから点jへの距離data[i,j]の和が点jのラベルlabel[j]よりも大きい場合
				if((label[i]+data[i,j])>label[j]){
					#点jのラベルlabel[j]を前者に変更する
					label[j]=label[i]+data[i,j];
					#点jへのパスpath[j]を,点iへのパスpath[i]に点jを左から加えたものとして更新する
					path[j]=paste(path[i],j);
				};	
			};	
		};	
	};
	#処理後のラベルベクトル
	y=label
	#ラベルベクトルが収束するまで(処理前後のラベルベクトルが等しくなるまで)
	if(rowSums(x)==rowSums(y)){break}
};

cat("距離行列");cat("\n");
print(data);
cat("スタート点");cat(s);cat("からの各点への最短距離");cat("\n");
print(label);
cat("パス");cat("\n");
print(path);