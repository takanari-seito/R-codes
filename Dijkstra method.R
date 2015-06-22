#�ŒZ�������̃_�C�N�X�g���@�ɂ���@
#input	:n*n�̋����s��
#output	:�X�^�[�g�_s����̊e�_�ւ̍ŒZ����(label;NA�͓��B�s�\)�ƃp�X(path)

#n*n�̋����s����G�N�Z������擾����
data=read.table("spp1.txt");

#n�������s��̃T�C�Y(�_��)nrow(data)�ɐݒ肷��
n=nrow(data);

rownames(data)=c(1:n);
colnames(data)=c(1:n);

#�e�_�ɑΉ�����T�C�Yn�̃��x���x�N�g��label���`����(�v�f�͂��ׂ�NA)
label=matrix(ncol=n);
colnames(label)=c(1:n);
for(i in 1:n){label[i]=NA};

#�e�_�ɑΉ�����T�C�Yn�̃p�Y�x�N�g��path���`����(�v�f�͊e�_)
path=matrix(ncol=n);
colnames(path)=c(1:n);
for(i in 1:n){path[i]=i};

#�X�^�[�g�_s�̐ݒ�A�X�^�[�g�_s�̃��x���x�N�g��label[s]��0�ɐݒ肷��
s=1;
label[s]=0;

x=0;
#x(�ŒZ�̃p�X��)��Inf�ɂȂ�܂ő�����(���x���\�Ȃ��ׂĂ̓_�����x���t�������܂�)
while(is.finite(x)){
	x=Inf;y=Inf;z=Inf;a=0;b=0;l=0;
	for(i in 1:n){
		#���x���̂��Ă��邠��_i��
		if((!is.na(label[i]))){
			for(j in 1:n){
				#�_i�ƌ������Ă���,�����x���̂��Ă��Ȃ����ׂĂ̓_j�ɑ΂���
				if((is.finite(data[i,j]))&&(is.na(label[j]))){
					#�_i�̃��x��label[i]�ɓ_i����_j�ւ̋���data[i,j]�𑫂�
					y=label[i]+data[i,j];
					#�ŏ��l�����߂�
					z=min(x,y);
					#�ŏ��l�ƂȂ����g�ݍ��킹�ɑ΂��ē_i�Ɠ_a,�_j��_b�ɍX�V����
					if(x>y){a=i;b=j};
					x=z;
				};	
			};	
		};	
	};
	#�_b�̃��x��label[b]�����̍ŏ��l�̒l�Ƃ���
	label[b]=x;
	#�_b�ւ̃p�Xpath[b]�ɓ_a�ւ̃p�Xpath[a]�������������
	path[b]=paste(path[a],path[b]);
};

cat("�����s��");cat("\n");
print(data);
cat("�X�^�[�g�_");cat(s);cat("����̊e�_�ւ̍ŒZ����");cat("\n");
print(label);
cat("�p�X");cat("\n");
print(path);