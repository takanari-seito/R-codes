###�j���[�����l�b�g���[�N�G�N���X���ށG�����͕ϐ��G���`���[�j���O###
#���͕ϐ���3���ȏ�Ȃ̂ŃO���t���͂ł��Ȃ�

#�f�[�^�̏���
all.data <- iris				#data.frame,�S�f�[�^�̏�������,iris�͂��Ƃ���R�ɓ����Ă���f�[�^
data <- all.data				#data.frame,��͂��鍀�ڂ̑I��,����͑S���ڂ�p����
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