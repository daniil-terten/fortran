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
      call Sort_Users_List(Users)
      call Output_Users_List(output_file, Users, "сортированный список:", "append")
      call Sort_Users_List_Descending(Users)
      call Output_Users_List(output_file, Users, "сортированный список в обратном порядке:", "append")


      !call Output_Ordered_List(output_file, Head, "Исходный список:", "rewind", 1)
      !call Output_Ordered_List(output_file, Head_Alph, "Список в алфавитном порядке:", "append", 2)
   end if
   
end program lab_3
