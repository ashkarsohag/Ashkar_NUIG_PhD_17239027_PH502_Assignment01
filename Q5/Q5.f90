

! Solution to the Question number (5)

! depends on the 3 subroutines written on separate files and located in the same directory

! Numerical Integration using 3 different rules (Trapezoidal, Simpson, and Gauss Quadrature)

!   Q_5(a): Separate functions have been created for each method

!   Q_5(b): A Makefile has been created to link 4 different *.f90 files
!           .... located in the same directory


program numericalIntegration
	implicit none
	integer :: n
	real :: area, a, b, actarea
	integer :: pick

!   Q_5(c): Allowing the user to pick the method and number of points

	write(*,*) 'Select the integration method from below:'
	write(*,*) 'Enter 1 for Trapezoidal method'
	write(*,*) 'Enter 2 for Simpson method'
	write(*,*) 'Enter 3 for Gauss Quadrature method'
	read(*,*) pick

!   Find the area under the curve fx=sin(x) from 0 to pi using Trapezoidal Rule

	if(pick==1) then
		write(*,*) ''
		write(*,*) 'Enter the number of points (n) [integer number]'
		read(*,*) n
		write(*,*) ''
	    write(*,*) 'So, the number of points (n) to be used for calculating the area'
	    write(*,*) 'under the curve [fx=sin(x) from 0 to pi] =',n
		
		call trapezoidal(n,area)
		
		write(*,*) ''
	    write(*,*) 'This program calculates the area with'
	    write(*,*)  n,'points is',area
	end if

!   Find the area under the curve fx=sin(x) from 0 to pi using Simpson Rule

	if(pick==2) then
	
	    write(*,*) ''
		write(*,*) 'Enter the number of points (n) [even integer number]'
		read(*,*) n
		
		do while(mod(n,2)==1)
		write(*,*) ''
		write(*,*) 'Enter the number of points (n) [even integer number]'
		read(*,*) n
		end do
		
		write(*,*) ''
	    write(*,*) 'So, the number of points (n) to be used for calculating the area'
	    write(*,*) 'under the curve [fx=sin(x) from 0 to pi] =',n

		call simpson(n,area)
		
		write(*,*) ''
	    write(*,*) 'This program calculates the area with'
	    write(*,*)  n,'points is',area
	end if

!   Find the area under the curve fx=sin(x) from 0 to pi using Gauss Quadrature Rule

	if(pick==3) then
		call gauss(area)
		write(*,*) ''
	    write(*,*) 'This program calculates the area with'
	    write(*,*) '2 points is',area
	end if

!	Calculation of the actual area
	a=0.0
	b=3.1415926535898
	actarea=(-cos(b))-(-cos(a))
	write(*,*) 'The actual value is', actarea
	

end program numericalIntegration


