program main
        use Environment

        implicit none
        character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                    :: In = 0, Out = 0, N = 0
        real(R_), allocatable      :: A(:)
        integer                    :: I

        open (file=input_file, newunit=In)
                read (In, *) N
                allocate (A(N))
                read (In, *) A
        close (In)

        A = A(N:1:-1)

        open (file=output_file, encoding=E_, newunit=Out)
                write (Out, "("//N//"F8.4)") (A(I), I=1,N)
        close (Out)
end program main
