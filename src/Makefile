
all : link_LAM_pilots.Abs

clean : 
	rm -f *.o *.Abs

link_LAM_pilots.Abs : link_LAM_pilots.F90
	s.f90 link_LAM_pilots.F90 -lrmn -o link_LAM_pilots.Abs
