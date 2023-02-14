program lab_3
   use Environment
   use List_IO

   implicit none
   character(:), allocatable        :: input_file, output_file
   type(User), pointer              :: Users => Null()

   input_file  = "../data/input.txt"
   output_file = "output.txt"

   Users => Read_Users_List(input_file)
   
   if (Associated(Users)) then
      call Output_Users_List(output_file, Users, "Исходный список:", "rewind")
      call Sort_Files_List_by_Extension(Users)
      call Output_Users_List(output_file, Users, "Отсортированный список по расширению:", "append")
      call Sort_Files_List_by_FileName(Users)
      call Output_Users_List(output_file, Users, "Отсортированный список по имени:", "append")


      !call Output_Ordered_List(output_file, Head, "Исходный список:", "rewind", 1)
      !call Output_Ordered_List(output_file, Head_Alph, "Список в алфавитном порядке:", "append", 2)
   end if
   
end program lab_3
