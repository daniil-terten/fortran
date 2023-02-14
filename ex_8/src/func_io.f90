module Func_IO
      use Environment

      implicit none
      integer           :: In = 0, Out = 0
contains
      function Read_matrix(input_file) result(A)
         character(*), intent(in)  :: input_file
         real(R_), allocatable     :: A(:, :)

         integer                   :: N = 0
         
         open (file=input_file, newunit=In)
            read (In, *) N
            allocate (A(N, N))
            read (In, *) A
         close (In)

      end function Read_matrix
      
      subroutine Write_result(output_file, res)
         character(*), intent(in)      :: output_file
         real(R_), intent(in)          :: res

         open (file=output_file, encoding=E_, newunit=Out)
            write (Out, "(a, f10.4)")  "Норма матрицы = ", res
         close (Out)
      end subroutine Write_result
end module Func_IO
