program lab_1_3
   use Environment
   use Users_Process
   use Users_IO
   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   
   type(Users)              :: Usrs(USER_AMOUNT)
   type(Users)              :: Usrs_Res(2)

   input_file  = "../data/input.txt"
   output_file = "output.txt"
   data_file   = "input.dat"
   
   call Create_Data_file(input_file, data_file)
   
   Usrs = Read_Users_List(data_file)

   call Output_Users_List(output_file, Usrs, "Исходный список:", "rewind")

   Usrs_Res =  Handle_Users_List(Usrs)

   call Output_Users_List(output_file, Usrs_Res, "Обработанный список:", "append")

end program lab_1_3
