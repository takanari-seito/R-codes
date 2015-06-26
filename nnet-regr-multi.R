###�j���[�����l�b�g���[�N�G�N���X���ށG�����͕ϐ��G���`���[�j���O###
#���͕ϐ���2�����ȏゾ�ƃO���t���ł��Ȃ�

#�f�[�^�̏���
all.data <- iris				#data.frame,�S�f�[�^�̏�������,iris�͂��Ƃ���R�ɓ����Ă���f�[�^
data <- all.data[,1:4]			#data.frame,��͂��鍀�ڂ̑I��,����͐��l�f�[�^��1~4���g�p���A4��ړI�ϐ��Ƃ���
n <- nrow(data)				#data�̃T���v�����̒��o
ids <- sample(n,n*0.8)			#data�̂���8���������_���ɃT���v�����O���P���f�[�^�Ƃ���
tdata <- data[ids,]
pdata <- data[-ids,]			#�c���2����\���ɗp����(�e�X�g�f�[�^)

#NN�̌P��	*�ڕW�ϐ��̐ݒ�,���߂Ă̍ۂ�install.packages("nnet")�Ńp�b�P�[�W���C���X�g�[������
library(nnet)
nn <- nnet(Petal.Width~.,tdata,size=10,linout=T)		#���`���[�j���O,linout=T�ŉ�A

#���m�̃f�[�^�̗\�z	*�����ɏ���Ă���قǎ��ۂ̒l�Ɨ\�z�̒l���߂�
prediction <- predict(nn,pdata)
plot(pdata$Petal.Width,prediction,xlab="pdata",ylab="prediction",xlim=c(0,3),ylim=c(0,3))
x <- 0:3
t <- x
par(new=T)
plot(x,t,type="l",xlab="",ylab="",xlim=c(0,3),ylim=c(0,3))