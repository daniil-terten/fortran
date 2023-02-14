module List_IO
   use Environment
   
   implicit None

   integer, parameter                  :: MAX_LEN = 15, CH_LEN = 1
   type User
      character(MAX_LEN, kind=CH_)     :: Last_Names  = "", FileName = "", Extension = ""
      type(User), pointer              :: next        => Null()
   end type User

contains
   pure recursive subroutine Sort_Files_List_by_Extension(Users)
      type(User), pointer              :: Users
      if (Associated(Users)) then
         call Sort(Users, Users, Users)
         call Sort_Files_List_by_Extension(Users%next)
      end if
   end subroutine Sort_Files_List_by_Extension

   pure recursive subroutine Sort(current, HeadMaxValue, Head)
      type(User), pointer                               :: HeadMaxValue, Head, current, tmp, tmp2
      intent(Out)                                       :: Head

      if (Associated(current)) then
         if (current%Extension > HeadMaxValue%Extension) then
            call Sort(current%next, current, Head)
         else
            call Sort(current%next, HeadMaxValue, Head)
         end if
      else
         allocate (tmp, source=Head)
         tmp%next          => HeadMaxValue%next
         tmp2 => HeadMaxValue
         HeadMaxValue => tmp
         tmp2%next   => Head%next
         Head => tmp2
      end if
   end subroutine Sort

   pure recursive subroutine Sort_Files_List_by_FileName(Users)
      type(User), pointer              :: Users
      if (Associated(Users)) then
         call Sort_by_FileName(Users, Users, Users)
         call Sort_Files_List_by_FileName(Users%next)
      end if
   end subroutine Sort_Files_List_by_FileName

   pure recursive subroutine Sort_by_FileName(current, HeadMaxValue, Head)
      type(User), pointer                               :: HeadMaxValue, Head, current, tmp, tmp2
      intent(Out)                                       :: Head

      if (Associated(current)) then
         if (current%FileName > HeadMaxValue%FileName) then
            call Sort_by_FileName(current%next, current, Head)
         else
            call Sort_by_FileName(current%next, HeadMaxValue, Head)
         end if
      else
         allocate (tmp, source=Head)
         tmp%next          => HeadMaxValue%next
         tmp2 => HeadMaxValue
         HeadMaxValue => tmp
         tmp2%next   => Head%next
         Head => tmp2
      end if
   end subroutine Sort_by_FileName

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
      integer                    :: IO, tmpIndex
      character(:), allocatable  :: format
      character(CH_LEN, kind=CH_)     :: Substr = "."

      allocate (Usr)
      format = "(a)"
      read (In, format, iostat=IO) Usr%Last_Names

      tmpIndex = INDEX(Usr%Last_Names, Substr)
      Usr%FileName = Usr%Last_Names(:tmpIndex-1)
      Usr%Extension = Usr%Last_Names(tmpIndex+1:)

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
      write (Out, format, iostat=IO) Usr%Last_Names
      call Handle_IO_status(IO, "writing user")
      if (Associated(Usr%next)) &
         call Output_User(Out, Usr%next)
   end subroutine Output_User


end module List_IO
