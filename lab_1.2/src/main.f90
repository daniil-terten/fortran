program lab_1_1
   use Environment

   implicit none
   integer, parameter                       :: USER_AMOUNT = 14, LN_LEN = 15, G_LEN = 1
   character(kind=CH_), parameter           :: MALE = Char(1052, CH_)

   character(:), allocatable                :: input_file, output_file
   integer                                  :: birthYear(USER_AMOUNT)
   integer                                  :: indexBoy(1), indexGirl(1)

   character(LN_LEN, kind=CH_)              :: Last_Names(USER_AMOUNT)
   character(G_LEN, kind=CH_)               :: Gender(USER_AMOUNT)

   input_file = "../data/input.txt"
   output_file = "output.txt"

   call Read_User_List(input_file, Last_Names, birthYear, Gender)
   call Write_User_List(output_file, Last_Names, birthYear, Gender)
   call Handle_User_List(birthYear, Gender, indexBoy, indexGirl)
   call Write_Res(output_file, Last_Names, birthYear, indexBoy, indexGirl)

   contains
      subroutine Read_User_List(Input_File, Last_Names, birthYear, Gender)
         character (*)                       :: Input_File
         character(LN_LEN, kind=CH_)         :: Last_Names(USER_AMOUNT)
         character(G_LEN, kind=CH_)          :: Gender(USER_AMOUNT)
         integer                             :: In, IO, i, birthYear(USER_AMOUNT)
         character(:), allocatable           :: format

         intent (In)          Input_File
         intent (Out)         Last_Names, birthYear, Gender

         open (file=Input_File, encoding=E_, newunit=In)
            format = "(a, i4, a)"
            read (In, format, iostat=IO) (Last_Names(i), birthYear(i), Gender(i), i=1, USER_AMOUNT)
            call Handle_IO_Status(IO, "reading users list")
         close (In)

      end subroutine Read_User_List

      subroutine Write_User_List(Output_File, Last_Names, birthYear, Gender)
         character (*)                       :: Output_File
         character(LN_LEN, kind=CH_)         :: Last_Names(USER_AMOUNT)
         character(G_LEN, kind=CH_)          :: Gender(USER_AMOUNT)
         integer                             :: Out, IO, i, birthYear(USER_AMOUNT)
         character(:), allocatable           :: format

         intent (in)         Output_File, Last_Names, birthYear, Gender

         open (file=Output_File, encoding=E_, newunit=Out)
            format = "(a, i4, a)"
            write (Out, "(a)") "Исходный список:"
            write (Out, format, iostat=IO) (Last_Names(i), birthYear(i), Gender(i), i=1, USER_AMOUNT)
            call Handle_IO_status(IO, "writing Users")
         close (Out)
      end subroutine Write_User_List

      subroutine Handle_User_List(birthYear, Gender, indexOfBoy, indexOfGirl)
         character(G_LEN, kind=CH_)          :: Gender(USER_AMOUNT)
         integer                             :: birthYear(USER_AMOUNT), indexOfBoy(1), indexOfGirl(1)

         intent (in)         birthYear, Gender
         intent (out)        indexOfBoy, indexOfGirl

         indexOfBoy     = MINLOC(birthYear, MASK = (Gender == MALE), DIM=1)
         indexOfGirl    = MAXLOC(birthYear, MASK = (.not. Gender == MALE), DIM=1)
      end subroutine Handle_User_List

      subroutine Write_Res(Output_File, Last_Names, birthYear, indexOfBoy, indexOfGirl)
         character (*)                       :: Output_File
         character(LN_LEN, kind=CH_)         :: Last_Names(USER_AMOUNT)
         integer                             :: Out, IO, birthYear(USER_AMOUNT), indexOfBoy(1), indexOfGirl(1)

         intent (in)         Output_File, Last_Names, birthYear, indexOfBoy, indexOfGirl

         open(file=Output_File, encoding=E_, position="append", newunit=Out)
            write (Out, "(/a)") "Самый пожилой мужчина:"
            write (Out, '(a, i4)', iostat=IO) Last_Names(indexOfBoy), birthYear(indexOfBoy)
            write (Out, "(/a)") "Самая молодая женщина:"
            write (Out, '(a, i4)', iostat=IO) Last_Names(indexOfGirl), birthYear(indexOfGirl)
            call Handle_IO_status(IO, "writing result")
         close (Out)

      end subroutine Write_Res


end program lab_1_1
