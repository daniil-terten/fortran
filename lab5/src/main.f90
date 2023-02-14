program main
        use Environment

        implicit none
        character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                 :: In = 0, Out = 0, N=0, I=0
        real                    :: a, b, result=0, h
        real(R_), allocatable   :: X(:), F(:)


        open (file=input_file, newunit=In)
                read (In, *) a, b, N
        close (In)

        allocate (X(N-1), F(N-1))

        h = (b-a)/N

        X = [((a + h*I), I = 1,N-1)]

        F = ff(X)

        result = (ff(a)+(4*SUM(F(1:N-1:2)))+(2*SUM(F(2:N-1:2)))+ff(b)) * (h/3)

        open (file=output_file, encoding=E_, newunit=Out)
                write (Out, '(a, f21.18, a, f7.4)') "Результат:", result, ", h =",  h
        close (Out)
contains
        pure elemental function ff(xx)
                real            :: xx, ff
                real,parameter  :: ex = 2.72, pi = 3.14

                intent(in) xx

                ff = ((ex**xx) * sin(pi * xx + .5_R_))
        end function ff
end program main
