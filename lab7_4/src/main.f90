program main
        use Environment

        implicit none
        character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                 :: In = 0, Out = 0, N=0, M=0, I, J
        real(R_), allocatable   :: A(:,:)

        open (file=input_file, newunit=In)
            read (In, *) N, M
            allocate (A(M, N))
            read (In, *) A
        close (In)
        

        call sort(A)

        open (file=output_file, encoding=E_, newunit=Out)
!            write (Out, "("//N//"F8.4)") (res(I), I=1,N)
            do I = 1, N
                write(Out, "("//M//"F8.4)") (A(J, I), J=1,M)
            end do
        close (Out)

     contains
        pure subroutine sort(X)
                real(R_), intent(inout) :: X(:,:)
                integer                 :: I, J, array_length, R
                real(R_)                :: buf

                array_length = SIZE(X, DIM=1)
                do R = 1, SIZE(X, DIM=2)
                  do I = 1, array_length
                           do J = (I+1), array_length
                                 if(X(I,R)>X(J,R)) then
                                          buf = X(I,R)
                                          X(I,R) = X(J,R)
                                          X(J,R) = buf
                                 end if
                           end do
                  end do
                end do
        end subroutine sort
end program main
