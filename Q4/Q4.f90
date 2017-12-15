
! Solution to the Question number (4)
! Numerical Integration using Simpson's Rule

!   Q_4(a): Find the area under the curve y=tan(x) from 0 to pi/3

program area_curve
	implicit none
	integer :: n,i,j
	real :: pi,a,b,h,area,sumodd,sumeven,fx,actrslt,dev, nn

!	Q_4(b): User input of an odd integer number and check if odd or even
n=0
do while(mod(n,2)==0)
		write(*,*) ''
		write(*,*) 'Q_4(b): Enter an odd integer number'
		read(*,*)   n
	end do
	    write(*,*) ''
	    write(*,*) 'So, the number of points (n) to be used for calculating the area'
	    write(*,*) 'under the curve [y=tan(x) from 0 to pi/3] =',n

	pi = 3.1415926535898
	a=0.0
	b=pi/3
	h=(b-a)/n
	
!	Summation of the odd points
sumodd=0
	do i=1,n,2
		fx=tan(a+h*i)
		sumodd=sumodd+fx
	end do

!	Summation of the even points
sumeven=0
	do i=2,n-1,2
		fx=tan(a+h*i)
		sumeven=sumeven+fx
	end do

!	Area calculation:
	area=h/3*(tan(a)+4*sumodd+2*sumeven+tan(b))
	write(*,*) ''
	write(*,*) 'NOTE: Given actual result for the closest to'
	write(*,*) 'less than or equal to 1000 points (n) is ln(2) (= 0.693147181)'
	write(*,*) ''
	write(*,*) 'Q_4(a): This program calculates the area with'
	write(*,*)  n,'points is',area
	
	actrslt=0.693147181
	
	dev=abs(actrslt-area)
	
	write(*,*) ''
	write(*,*) 'Q_4(c): The area resulted thereby is within' 
	write(*,*) dev,'of the actual result ln(2) (see NOTE above)'


end program area_curve
