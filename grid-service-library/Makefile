# On gateway:
# module purge ; module load cineca ; module load imasenv
# On SDCC:
# module purge : module load --auto IMAS/3.37.1-4.11.0-2020b GGD/1.10.2-intel-2020b-DD-3.37.0

GGD_FLAGS=$(shell pkg-config ggd --libs --cflags )


write_b3d_ggd : write_b3d_ggd.f90 b3d_ggd.o
	imasdb "ggdtest"
	ifort -o $@ $^ $(GGD_FLAGS)
	./$@


b3d_ggd.o : b3d_ggd.f90
	ifort -c $^ $(GGD_FLAGS)


