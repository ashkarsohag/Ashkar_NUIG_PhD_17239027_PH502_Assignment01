
! Solution to the Question number 5(e)

program numericalIntegration

implicit none
integer :: pick, k(4)=(/2, 8, 16, 64/), i
real :: area, a, b, actarea

open (unit = 7, file = "Q5e_area_tabulated.txt")

pick=0

do while (pick<3)
    pick = pick+1
        if (pick==1) then
        write (*,*) ''
        write (*,*) ''
            write (7,*) 'Area of the curve using Trapezoidal Method : '
            area = 0.
            do i=1,size(k)
            call trapezoidal(k(i), area)
            write (*,*) ''
            write (7,*) 'Intervals/points, n = ',k(i),'and the corresponding area =', area
            end do
        else if (pick==2) then
        write (*,*) ''
        write (*,*) ''
            write (7,*) 'Area of the curve using Simpson Method : '
            area = 0.
            do i=1,size(k)
            call simpson(k(i), area)
            write (*,*) ''
            write (7,*) 'Intervals/points, n = ',k(i),'and the corresponding area =', area
            end do
        else if (pick==3) then
        write (*,*) ''
        write (*,*) ''
            write (7,*) 'Area of the curve using Gauss Method : '
            call gauss(area)
            write (*,*) ''
            write (7,*) 'Intervals/points, n = ',k(1),'and the corresponding area =', area
        
        end if
        
        
end do

!	Calculation of the actual area
	a=0.0
	b=3.1415926535898
	actarea=(-cos(b))-(-cos(a))
	write (*,*) ''
	write (*,*) ''
	write(7,*) 'The actual value of the area is', actarea

close (7)

end program numericalIntegration



!	Subroutine for Trapezoidal rule
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


!	Subroutine for Simpson's rule
subroutine simpson(n,area)
integer :: i
real :: pi,a,b,h,sum1,sum2,fx
pi = 3.1415926535898
a=0.0
b=pi
h=(b-a)/n	
!	the sum of odd points
sum1=0
do i=1,n,2
fx=sin(a+h*i)
sum1=sum1+fx
end do
!	the sum of even points
sum2=0
do i=2,n-1,2
fx=sin(a+h*i)
sum2=sum2+fx
end do
!	final result
area=h/3*(sin(a)+4*sum1+2*sum2+sin(b))
	
end

!	Subroutine for Guass quadrature rule
subroutine gauss(area)
real :: pi,a,b,h,sum
pi = 3.1415926535898
a=0.0
b=pi
h=(b-a)/2
sum=sin(a+h-h/sqrt(3.0))+sin(a+h+h/sqrt(3.0))	
area=h*sum
end 

