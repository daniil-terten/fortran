program main
        use Environment

        implicit none
        character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                 :: In = 0, Out = 0, N=0, I
        real(R_), allocatable   :: A(:)
        
        open (file=input_file, newunit=In)
                read (In, *) N
                allocate (A(N))
                read (In, *) A
        close (In)

        call ff(A)

        open (file=output_file, encoding=E_, newunit=Out)
                write (Out, "("//N//"F8.4)") (A(I), I=1,N)
        close (Out)
        
     contains
        pure subroutine ff(X)
                real(R_), intent(inout) :: X(:)
                integer                 :: I, J, array_length
                real(R_)                :: buf

                array_length = SIZE(X)
                do I = 1, array_length
                        do J = (I+1), array_length
                                if(X(I)>X(J)) then
                                        buf = X(I)
                                        X(I) = X(J)
                                        X(J) = buf
                                end if
                        end do
                end do
        end subroutine ff
end program main
