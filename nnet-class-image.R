###�j���[�����l�b�g���[�N�G�N���X���ށG2���͕ϐ��G�O���t���G���`���[�j���O###
#�O���t������ۂɂ͓��͕ϐ��Ƃ���1,2�ϐ������p���邱�Ƃ��ł��Ȃ����Ƃɒ���,�o�͂͐ݒ肵���N���X�̊m��

#�f�[�^�̏���
all.data <- iris				#data.frame,�S�f�[�^�̏�������,iris�͂��Ƃ���R�ɓ����Ă���f�[�^
data <- all.data[,c(1,3,5)]		#data.frame,��͂��鍀�ڂ̑I��,���͕ϐ��Ƃ���1,3���,�ڕW�ϐ��Ƃ���5��ڂ�I��
n <- nrow(data)				#data�̃T���v�����̒��o
ids <- sample(n,n*0.8)			#data�̂���8���������_���ɃT���v�����O���P���f�[�^�Ƃ���
tdata <- data[ids,]
pdata <- data[-ids,]			#�c���2����\���ɗp����(�e�X�g�f�[�^)

#NN�̌P��	*�ڕW�ϐ��̐ݒ�,���߂Ă̍ۂ�install.packages("nnet")�Ńp�b�P�[�W���C���X�g�[������
library(nnet)
nn <- nnet(Species~.,tdata,size=10)		#���`���[�j���O

#�����ނ̃f�[�^�̗\��
prediction <- predict(nn,pdata,type="class")	#type����͂��Ȃ��Ɗm�����o�͂����
table(prediction,pdata$Species)

###�ȉ��O���t��###
#�������O���t�̏���(����͈̔͂̓_���ʂɍ��Ann�ɑ�����ăv���b�g����)
l <- 100					#���̕�����(1000���炢����x���Ȃ�)
input1 <- seq(4,9,length=l)						#vector,�͈͂͌P���f�[�^�݂̂̃v���b�g�͈̔͂��Q�l��
input2 <- seq(0,8,length=l)						#vector
input <- expand.grid(input1,input2)					#data.frame
names(input) <- c("Sepal.Length","Petal.Length")		#nn�͗�̖��O���Q�l�ɓ��͕ϐ���T���Ă���?
output <- predict(nn,input)[,1]					#data.frame,�o�͂���N���X���w��
output <- matrix(output,l:l)						#matrix
cols <- colorRampPalette(c("white","red"))			#�F�̐ݒ�
image(input1,input2,output,xlab="Sepal.Length",ylab="Petal.Length",
	xlim=c(4,9),ylim=c(0,8),col=cols(5))			#image(x1=vector,x2=vector,t=matrix)????

#�P���f�[�^�̃v���b�g
data1 <- subset(tdata,Species=="setosa")				#data.frame,���ꂼ��̕i��݂̂��܂ރf�[�^�̍쐬
data2 <- subset(tdata,Species=="versicolor")
data3 <- subset(tdata,Species=="virginica")
x11 <- data1[,1];x12 <- data1[,2]					#data.frame,���ꂼ��̓��͕ϐ��̒��o
x21 <- data2[,1];x22 <- data2[,2]
x31 <- data3[,1];x32 <- data3[,2]
par(new=T)	#�O���t���d�˂�
plot(x11,x12,xlim=c(4,9),ylim=c(0,8),xlab="",ylab="",pch=1)
par(new=T)
plot(x21,x22,xlim=c(4,9),ylim=c(0,8),xlab="",ylab="",pch=20)
par(new=T)
plot(x31,x32,xlim=c(4,9),ylim=c(0,8),xlab="",ylab="",pch=4)





#�P���f�[�^�̂�,�͈͂̐ݒ�̎Q�l��
x1 <- data[,1]
x2 <- data[,2]
plot(x1,x2)