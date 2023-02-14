program exercise_8
   use Environment
   use Func
   use Func_IO

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   real(R_)                :: res
   real(R_), allocatable   :: A(:,:)
   
   A = Read_matrix(input_file)

   res = Matrix_norm(A)
   call Write_result(output_file, res)

end program exercise_8
