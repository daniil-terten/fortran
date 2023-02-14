program main
        use Environment

        implicit none
        character(*), parameter    :: input_file = "../data/input.txt", output_file = "output.txt"
        integer                    :: In = 0, Out = 0, a
        character(:), allocatable  :: result

        open (file=input_file, newunit=In)
                read (In, *) a
        close (In)

        result = Parity_check(a)

        open (file=output_file, encoding=E_, newunit=Out)
                write (Out, "(1x, a, I5, a)") "Число ", a, result
        close (Out)

        contains
        pure function Parity_check(x)
                implicit none
                integer, intent (in)       :: x
                character(:), allocatable  :: parity_check

                if((MOD(x, 2)) > 0) then
                        parity_check = " нечетное"
                else
                        parity_check = " четное"
                end if
        end function Parity_check
end program main
