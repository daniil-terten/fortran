program main
        use Environment

        implicit none
        character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                 :: In = 0, Out = 0, N=0
        real(R_)                :: a, b, result=0, h
        real(R_),parameter      :: ex = 2.72, pi = 3.14
        real(R_), allocatable   :: X(:), F(:)

        open (file=input_file, newunit=In)
                read (In, *) a, b, h
        close (In)

        N = Int((b-a)/h)
        allocate (X(N), F(N))

        call Get_interpolation_points(a, h, X)

        F = [(ex**X) * sin(pi * X + .5_R_)]
        result = h * Sum(F)

        open (file=output_file, encoding=E_, newunit=Out)
                write (Out, '(f21.18)') result
        close (Out)
contains
        pure subroutine Get_interpolation_points(x1, h, X)
                real(R_)    x1, h, X(:)

                intent(in)  x1, h
                intent(out) X

                integer     i

                X = [(x1 + h*i, i = 1, Size(X))]
        end subroutine Get_interpolation_points
end program main
