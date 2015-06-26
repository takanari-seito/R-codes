###�j���[�����l�b�g���[�N�G�N���X���ށG2���͕ϐ��G�O���t���G���`���[�j���O###
#�O���t������ۂɂ͓��͕ϐ��Ƃ���1�ϐ������p���邱�Ƃ��ł��Ȃ����Ƃɒ���

#�f�[�^�̏���
all.data <- iris				#data.frame,�S�f�[�^�̏�������,iris�͂��Ƃ���R�ɓ����Ă���f�[�^
tdata <- all.data[,c(1,3)]		#data.frame,��͂��鍀�ڂ̑I��,���͕ϐ��Ƃ���1���,�ڕW�ϐ��Ƃ���3��ڂ�I��

#NN�̌P��	*�ڕW�ϐ��̐ݒ�,���߂Ă̍ۂ�install.packages("nnet")�Ńp�b�P�[�W���C���X�g�[������
library(nnet)
nn <- nnet(Petal.Length~.,tdata,size=10,linout=T)		#���`���[�j���O,linout=T�ŉ�A

###�ȉ��O���t��###
#�������O���t�̏���(����͈̔͂̓_���ʂɍ��Ann�ɑ�����ăv���b�g����)
l <- 1000									#���̕�����(1000���炢����x���Ȃ�)
input1 <- seq(4,8,length=l)
input2 <- expand.grid(input1)						#data.frame,�͈͂͌P���f�[�^�݂̂̃v���b�g�͈̔͂��Q�l��
names(input2) <- "Sepal.Length"					#nn�͗�̖��O���Q�l�ɓ��͕ϐ���T���Ă���?
output <- predict(nn,input2)						#data.frame,predict(nn,x=data.frame)
plot(input1,output,xlab="Sepal.Length",ylab="Petal.Length",xlim=c(4,9),ylim=c(0,8),type="l")
										#type="l"�œ_���Ȃ�

#�P���f�[�^�̃v���b�g
x <- data[,1]
t <- data[,2]
par(new=T)
plot(x,t,xlab="",ylab="",xlim=c(4,9),ylim=c(0,8))


#�P���f�[�^�̂�,�͈͂̐ݒ�̎Q�l��
x <- tdata[,1]
t <- tdata[,2]
plot(x,t)