
!   Solution to the Question number (2)

!	Programme on Collatz_conjecture

Program CC
	implicit none
	integer :: n,count,i,j,k
	integer :: fn                     ! fn = function of n
	integer,dimension(10,10) :: seq   ! seq = sequence of numbers as function outputs 
	
	write (*,*) ''
	write (*,*) 'Solution to the Question number (2)'
	write (*,*) ''
	write(*,*) 'Q_2(a): Write a positive integer (and hit ENTER) '
	read(*,*) n


! Q_2(b): Collatz Conjecture function		
	
	count=0
	fn=0

	i=1
	do while(n/=1)
		count=count+1
		seq(i,count)=n

		if(mod(n,2)==0) then
			fn=n/2
		else if(mod(n,2)/=0) then
			fn=3*n+1
		end if		
		n=fn
		
! Q_2(c): Termination of the program while the function sequence is 4, 2, 1
		
		if(count==10.or.fn==1) then
			if(fn==1.and.count/=10)then
				seq(i,count+1)=1
			end if
			if(fn==1.and.count==10)then
				seq(i+1,1)=1
			end if
			count=0
			i=i+1
		end if
	end do


!	Q_2(d): printing output as a sequence

    write (*,*) ''
	write(*,*) 'Q_2(d): Collatz Conjecture Funtion output is '
	write (*,*) ''
	
	do i=1,10,1
		do j=1,10,1
			if(seq(i,j)==0) GO TO 10
		end do
		write(*,20) seq(i,1:10)
	end do

    

10	write(*,30) seq(i,1:j-1)
20  format(10(i3,',')/)
30  format(*(i3,',')/)

end program CC
