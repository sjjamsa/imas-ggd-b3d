# module purge ; module load cineca ; module load imasenv


GGD_FLAGS=$(shell pkg-config ggd --libs --cflags )


write_b3d_ggd : write_b3d_ggd.f90 b3d_ggd.o
	imasdb "ggdtest"
	ifort -o $@ $^ $(GGD_FLAGS)
	./$@


b3d_ggd.o : b3d_ggd.f90
	ifort -c $^ $(GGD_FLAGS)


