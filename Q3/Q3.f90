
!   Solution to the Question number (3)

!	Function that calculates the determinant of any 3x3 matrix
subroutine func33mat(mtrx,deter)
	real,dimension(3,3) :: mtrx
	deter=mtrx(1,1)*(mtrx(2,2)*mtrx(3,3)-mtrx(2,3)*mtrx(3,2))&
	      &-mtrx(1,2)*(mtrx(2,1)*mtrx(3,3)-mtrx(2,3)*mtrx(3,1))&
	      &+mtrx(1,3)*(mtrx(2,1)*mtrx(3,2)-mtrx(2,2)*mtrx(3,1))
end

Program matdeter         ! matdeter is the program to generate the matrix and to calculate its determinant
	implicit none
	real,dimension(3,3) :: mtrx
	real :: deter,d44h   ! deter is the determinant and d44h is the determinant of 4x4 hilbert matrix
	integer :: i,j
	real,dimension(4,4) :: hilbert

! Calculating the determinant of any 3x3 matrix
write (*,*) ''
write (*,*) 'Solution to the Question number (3)'
write (*,*) ''
write (*,*) 'Q_3(a) :'
write (*,*) '3x3 matrix determinant & 
          &= mtrx(1,1)*(mtrx(2,2)*mtrx(3,3)-mtrx(2,3)*mtrx(3,2))&
	      &-mtrx(1,2)*(mtrx(2,1)*mtrx(3,3)-mtrx(2,3)*mtrx(3,1))&
	      &+mtrx(1,3)*(mtrx(2,1)*mtrx(3,2)-mtrx(2,2)*mtrx(3,1))'

!	Hilbert matrix loop
	do i=1,4,1
		do j=1,4,1
			hilbert(i,j)=1.0/(i+j-1)
		end do
	end do

    write (*,*) ''
    write(*,*) 'Q_3(b) :'
	write(*,*) 'Hilbert matrix : '
	write (*,*) ''
	write(*,10) hilbert

!	Calculating the determinant of 4x4 Hilbert matrix using Cramer's Rule
	mtrx(:,:)=hilbert(2:4,2:4)
	call func33mat(mtrx,deter)
	d44h=d44h+hilbert(1,1)*deter

	mtrx(:,1)=hilbert(2:4,1)
	mtrx(:,2:3)=hilbert(2:4,3:4)
	call func33mat(mtrx,deter)
	d44h=d44h-hilbert(1,2)*deter

	mtrx(:,1:2)=hilbert(2:4,1:2)
	mtrx(:,3)=hilbert(2:4,4)
	call func33mat(mtrx,deter)
	d44h=d44h+hilbert(1,3)*deter

	mtrx(:,:)=hilbert(2:4,1:3)
	call func33mat(mtrx,deter)
	d44h=d44h-hilbert(1,4)*deter

	write(*,*) 'Q_3(c) :'
	write(*,*) 'The determinant of the 4x4 Hilbert matrix is'
		write(*,20) d44h



10	format(4(4f5.2/))
20	format(f10.8)

end program matdeter   	










