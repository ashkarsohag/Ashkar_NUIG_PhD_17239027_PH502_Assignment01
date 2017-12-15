
!   Part of the Solution to the Question number (5)

!	This is a Subroutine to be linked with numericalIntegration.f90

!   for calculating the area under a curve using the Gauss Quadrature Rule


subroutine trapezoidal(n,area)
	integer ::i
	real :: pi,a,b,h,sum,fx
	pi = 3.1415926535898
	a=0.0
	b=pi
	h=(b-a)/n
	sum=0
	do i=1,n
		fx=sin(a+h*i)
		sum=sum+fx		
	end do
	sum=2*sum+sin(a)+sin(b)
	area=h/2*sum
end 

