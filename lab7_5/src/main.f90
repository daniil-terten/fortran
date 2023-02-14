program main
        use Environment

        implicit none
        character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                 :: In = 0, Out = 0, N=0
        real(R_), allocatable   :: A(:), B(:,:)
        
        open (file=input_file, newunit=In)
                read (In, *) N
                allocate (A(N))
                read (In, *) A
        close (In)
        
        B = RESHAPE(A, (/(N/5), 5/))

        open (file=output_file, encoding=E_, newunit=Out)
                write(Out, "("//N/5//"F8.4)") B
        close (Out)
        
end program main
