
!   Part of the Solution to the Question number (5)

!	This is a Subroutine to be linked with numericalIntegration.f90

!   for calculating the area under a curve using the Gauss Quadrature Rule

subroutine gauss(area)
	real :: pi,a,b,h,sum
	pi = 3.1415926535898
	a=0.0
	b=pi
	h=(b-a)/2
	sum=sin(a+h-h/sqrt(3.0))+sin(a+h+h/sqrt(3.0))	
	area=h*sum
end 




