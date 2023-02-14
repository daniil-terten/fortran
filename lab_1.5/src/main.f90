program lab_1_4
   use Environment
   use Users_Process
   use Users_IO
   implicit none
   
   character(:), allocatable        :: input_file, output_file
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), FEMALE = Char(1046, CH_)
   type(User), pointer              :: Users => Null(), Boys_List => Null(), Girls_List => Null()
   integer                          :: Boys_Amount = 0, Girls_Amount = 0

   input_file  = "../data/input.txt"
   output_file = "output.txt"

   Users => Read_Users_List(input_file)

   if (Associated(Users)) then
      call Output_Users_List(output_file, Users, "Исходный список:", "rewind")

      call Get_list_by_gender(Users, Boys_List, Boys_Amount, MALE)
      call Get_list_by_gender(Users, Girls_List, Girls_Amount, FEMALE)

      call Delete_unnecessary_boys(Boys_List, Boys_List%next)
      call Delete_unnecessary_girls(Girls_List, Girls_List%next)

      call Output_Users_List(output_file, Boys_List, "Самый пожилой мужчина:", "append")
      call Output_Users_List(output_file, Girls_List, "Самая молодая женщина", "append")
   end if

end program lab_1_4
