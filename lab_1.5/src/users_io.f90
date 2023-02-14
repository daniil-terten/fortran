module Users_IO
   use Environment

   implicit none
   integer, parameter :: LN_LEN        = 15
   integer, parameter :: G_LEN         = 1
 
   type User
      character(LN_LEN, kind=CH_)              :: Last_Names           = ""
      character(G_LEN, kind=CH_)               :: Gender               = ""
      integer                                  :: birthYear            = 0
      type(User), pointer                      :: next                 => Null()
   end type User
   
contains
   function Read_Users_List(Input_File) result(Users_List)
      type(User), pointer        :: Users_List
      character(*), intent(in)   :: Input_File
      integer                    :: In

      open (file=Input_File, encoding=E_, newunit=In)
         Users_List => Read_User(In)
      close (In)
   end function Read_Users_List

   recursive function Read_User(In) result(Usr)
      type(User), pointer        :: Usr
      integer, intent(in)        :: In
      integer                    :: IO
      character(:), allocatable  :: format

      allocate (Usr)
      format = "(a, i4, a)"
      read (In, format, iostat=IO) Usr%Last_Names, Usr%birthYear, Usr%Gender
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Usr%next => Read_User(In)
      else
         deallocate (Usr)
         nullify (Usr)
      end if
   end function Read_User

   subroutine Output_Users_List(Output_File, Users, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      type(User), intent(in)     :: Users
      integer                    :: Out

      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_User(Out, Users)
      close (Out)
   end subroutine Output_Users_List

   recursive subroutine Output_User(Out, Usr)
      integer, intent(in)        :: Out
      type(User), intent(in)     :: Usr
      integer                    :: IO
      character(:), allocatable  :: format

      format = "(a, i4, a)"
      write (Out, format, iostat=IO) Usr%Last_Names, Usr%birthYear, Usr%Gender
      call Handle_IO_status(IO, "writing user")
      if (Associated(Usr%next)) &
         call Output_User(Out, Usr%next)
   end subroutine Output_User
 

end module Users_IO
