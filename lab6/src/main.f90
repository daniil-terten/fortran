program main
        use Environment

        implicit none
        character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                 :: In = 0, Out = 0
        real                    :: a, result

        open (file=input_file, newunit=In)
                read (In, *) a
        close (In)

        result = ff(a)

        open (file=output_file, encoding=E_, newunit=Out)
                write (Out, '(a, f12.8, a, f7.4)') "sqrt from ", a, " = ", result
                write (Out,'(f7.4)') sqrt(a)
        close (Out)
contains
        pure function ff(a)
                real            :: a, f_old, f_new, ff

                intent(in) a

                f_old = a
                do
                        f_new = f_old - ((f_old**2 - a)/(2*f_old))
                        if(ABS(f_new-f_old) == 0._R_) exit
                        f_old = f_new
                end do
                ff = f_new
        end function ff
end program main
