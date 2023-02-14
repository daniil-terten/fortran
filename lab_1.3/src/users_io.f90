module Users_IO
   use Environment

   implicit none
   integer, parameter :: USER_AMOUNT   = 14
   integer, parameter :: LN_LEN        = 15
   integer, parameter :: G_LEN         = 1
 
   type Users
      character(LN_LEN, kind=CH_)              :: Last_Names
      character(G_LEN, kind=CH_)               :: Gender
      integer                                  :: birthYear
   end type Users
   
contains
   subroutine Create_Data_File(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file
      
      integer                    :: In, Out, IO, i
      character(:), allocatable  :: format

      character(LN_LEN, kind=CH_)              :: Last_Names(USER_AMOUNT)
      character(G_LEN, kind=CH_)               :: Gender(USER_AMOUNT)
      integer                                  :: birthYear(USER_AMOUNT)
     
      open (file=Input_File, encoding=E_, newunit=In)
         format = "(a, i4, a)"
         read (In, format, iostat=IO) (Last_Names(i), birthYear(i), Gender(i), i=1, USER_AMOUNT)
         call Handle_IO_status(IO, "init arrays with formatted file")
      close (In)
      
      open (file=Data_File, form="unformatted", newunit=Out, access="stream")
         write (Out, iostat=IO) Last_Names, birthYear, Gender
         call Handle_IO_status(IO, "creating unformatted file by index")
      close (Out)
   end subroutine Create_Data_File

   function Read_Users_List(Data_File) result(Usrs)
      type(Users)                :: Usrs(USER_AMOUNT)
      character(*), intent(in)   :: Data_File
      integer                    :: In, IO
      
      open (file=Data_File, form="unformatted", newunit=In, access="stream")
         read (In, iostat=IO) Usrs%Last_Names, Usrs%birthYear, Usrs%Gender
         call Handle_IO_status(IO, "init objects")
      close (In)
   end function Read_Users_List
 
   subroutine Output_Users_List(Output_File, Usrs, List_name, Position)
      character(*), intent(in)   :: Output_File, List_name, Position
      type(Users), intent(in)    :: Usrs(:)

      integer                    :: Out, IO, i
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
            format = "(a, i4, a)"
            write (Out, "(a)") List_name
            write (Out, format, iostat=IO) (Usrs(i)%Last_Names, Usrs(i)%birthYear, Usrs(i)%Gender, i=1, Size(Usrs))
            call Handle_IO_status(IO, "writing"//List_name)
      close (Out)
   end subroutine Output_Users_List
end module Users_IO
