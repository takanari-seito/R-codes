#@@@�O���t�f�[�^(�|�C���^)���琮���v��@�̓��͌`���ւ̕ϊ�,�ŒZ�������̐����v��@��p������@@@@

###�f�[�^�t���[��w�ǂݍ���(��v->u;�d��w)###		Excel��ł��炩���߃\�[�g���Ă���
data <-read.table("graph3(pointer).txt")�@#�v�ύX
names(data) <-c("v","u","w")
#���_��n
n <-max(data$v,data$u)
#�Ӑ�ne
ne <-nrow(data)
#�n�_s
s <-1			#�v�ύX
#�I�_
t <-18		#�v�ύX

###LP�s��m###
#objective function
d <-matrix(0:0,ncol=(ne+2))
for(i in 1:ne){d[i]�@<-data[i,3]}
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

###�����v��p�̗l���ɕϊ�###
f.obj <-c(m[1,1:ne])
f.con <-m[2:(n+ne+1),1:ne]
f.dir <-m[2:(n+ne+1),(ne+1)]
f.rhs <-m[2:(n+ne+1),(ne+2)]
#�����v���������
library(lpSolve)
result <-lp("min",f.obj,f.con,f.dir,f.rhs,int.vec=1:ne)$solution	#"min"->"max"�ōŒ��������ɕύX�\(��H���܂܂Ȃ��ꍇ)
data <-cbind(data,result)
#�ŒZ����d
d=0
for(i in 1:ne){d <-d+data[i,3]*data[i,4]}
###�|�C���^�̍s��\��###
wm <-matrix(nrow=n,ncol=n)
wm[,] <-Inf
for(i in 1:ne){wm[(data[i,1]),(data[i,2])] <-data[i,3]}
em <-matrix(nrow=n,ncol=n)
em[,] <-0
for(i in 1:ne){if(data[i,4]==1){em[(data[i,1]),(data[i,2])] <-1}}
#����path
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


###���ʕ\��###
m
data
wm
em
d
path



