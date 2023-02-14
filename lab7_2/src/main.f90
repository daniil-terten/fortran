program main
        use Environment

        implicit none
        character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                 :: In = 0, Out = 0, N = 0, M = 0, I, J
        real(R_)                :: indexMin
        real(R_), allocatable   :: A(:, :), res(:)
        
        open (file=input_file, newunit=In)
                read (In, *) N, M
                allocate (A(N, M), res(N))
                read (In, *) A
        close (In)

        res = SUM(A, DIM=1)
        indexMin = MinLoc(res, DIM=1)

        open (file=output_file, encoding=E_, newunit=Out)
!            write (Out, "("//N//"F8.4)") (res(I), I=1,N)
            do I = 1, N
                write(Out, "("//M//"F8.4)") (A(J, I), J=1,M)
            end do
            write (Out, "(a ,F2.0)") "Минимальная сумма элементов в строке ", indexMin
        close (Out)

        
end program main
