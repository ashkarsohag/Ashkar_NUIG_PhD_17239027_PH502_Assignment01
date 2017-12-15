

# Makefile for linking 3 subroutines to the main program Q5

FC = gfortran
CODES = Q5.o trap_sr.o simp_sr.o gauss_sr.o


numericalIntegration: $(CODES)
	$(FC) -o numericalIntegration $(CODES)

Q5.o: Q5.f90
	$(FC) -c Q5.f90

trap_sr.o: trap_sr.f90
	$(FC) -c trap_sr.f90

simp_sr.o: simp_sr.f90
	$(FC) -c simp_sr.f90

gauss_sr.o: gauss_sr.f90
	$(FC) -c gauss_sr.f90


clean:
	rm *.o numericalIntegration


