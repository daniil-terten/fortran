program main
        use Environment

        implicit none
        character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                 :: In = 0, Out = 0, N = 0, M = 0, I, J, k, First, Last = 0
        real(R_)                :: indexMin
        real(R_), allocatable   :: A(:, :), res(:)
        integer, allocatable    :: Initial(:), Quantity(:)
        
        open (file=input_file, newunit=In)
                read (In, *) N, M, k
                allocate (A(N, M), res((N*M+1)/3), Initial(0:k-1), Quantity(0:k-1))
                read (In, *) A
        close (In)


        Initial(:) = k - [(I, I = 0, k-1)]
        Quantity(:) = (N + [(I, I = 0, k-1)])/k-1

        do j = 1, M
                I = MOD(j, k)
                First = Last + 1
                Last = First + Quantity(I)
                res(First:Last) = A(Initial(I)::k, j)
        end do


        open (file=output_file, encoding=E_, newunit=Out)
           ! do I = 1, (N*M+1)/3
                write(Out, "("//M//"F8.4)") res
           ! end do
        close (Out)


end program main
