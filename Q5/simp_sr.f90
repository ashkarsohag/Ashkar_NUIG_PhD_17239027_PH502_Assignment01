
!   Part of the Solution to the Question number (5)

!	This is a Subroutine to be linked with numericalIntegration.f90

!   for calculating the area under a curve using the Gauss Quadrature Rule


subroutine simpson(n,area)
	integer :: i
	real :: pi,a,b,h,sum1,sum2,fx
	pi = 3.1415926535898
	a=0.0
	b=pi
	h=(b-a)/n	
!	Summation of the odd points
    sum1=0
	do i=1,n,2
		fx=sin(a+h*i)
		sum1=sum1+fx
	end do
!	Summation of the even points
    sum2=0
	do i=2,n-1,2
		fx=sin(a+h*i)
		sum2=sum2+fx
	end do
!	final result
	area=h/3*(sin(a)+4*sum1+2*sum2+sin(b))
	
end
