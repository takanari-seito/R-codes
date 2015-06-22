#�Œ��������̃|�e���V�����A�b�v�@�ɂ���@
#input	:n*n�̋����s��(���[�v�����݂��Ȃ�)
#output	:�X�^�[�g�_s����e�_�ւ̍Œ�����(label;NA�͓��B�s�\)�ƃp�X(path)


#####�X�^�[�g�_��ς��邱�Ƃ��ł��Ă��Ȃ�#####


#n*n�̋����s����G�N�Z������擾����
data=read.table("lpp1.txt");

#n�������s��̃T�C�Y(�_��)nrow(data)�ɐݒ肷��
n=nrow(data);

rownames(data)=c(1:n);
colnames(data)=c(1:n);

#�e�_�ɑΉ�����T�C�Yn�̃��x���x�N�g��label���`����(�v�f�͂��ׂ�0)
label=matrix(ncol=n);
colnames(label)=c(1:n);
for(i in 1:n){label[i]=0};

#�e�_�ɑΉ�����T�C�Yn�̃p�X�x�N�g�����`����(�v�f�͊e�_)
path=matrix(ncol=n);
colnames(path)=c(1:n);
for(i in 1:n){path[i]=i};

#�X�^�[�g�_s��ݒ�
s=1;

###���[�v���Ȃ����Ƃ̊m�F��ǉ������ق����悢###

repeat{
	#�����O�̃��x���x�N�g��
	x=label
	#����̓_i��
	for(i in 1:n){
		for(j in 1:n){
			#�_i�ƌ������Ă���_j�ɑ΂���
			if(is.finite(data[i,j])){
				#�_i�̃��x��label[i]�Ɠ_i����_j�ւ̋���data[i,j]�̘a���_j�̃��x��label[j]�����傫���ꍇ
				if((label[i]+data[i,j])>label[j]){
					#�_j�̃��x��label[j]��O�҂ɕύX����
					label[j]=label[i]+data[i,j];
					#�_j�ւ̃p�Xpath[j]��,�_i�ւ̃p�Xpath[i]�ɓ_j����������������̂Ƃ��čX�V����
					path[j]=paste(path[i],j);
				};	
			};	
		};	
	};
	#������̃��x���x�N�g��
	y=label
	#���x���x�N�g������������܂�(�����O��̃��x���x�N�g�����������Ȃ�܂�)
	if(rowSums(x)==rowSums(y)){break}
};

cat("�����s��");cat("\n");
print(data);
cat("�X�^�[�g�_");cat(s);cat("����̊e�_�ւ̍ŒZ����");cat("\n");
print(label);
cat("�p�X");cat("\n");
print(path);