module Users_Process
   use Environment
   use Users_IO

   implicit none
   
contains

   pure recursive subroutine Delete_unnecessary_girls(current, next)
      type(User), pointer                               :: current, tmp, next

      if (Associated(next)) then
         if ( current%birthYear < next%birthYear) then
            tmp => current

            current => next
            deallocate(tmp)
         end if
         call Delete_unnecessary_girls(current, next%next)
      else
         current%next => Null()
      end if
   end subroutine Delete_unnecessary_girls

   pure recursive subroutine Delete_unnecessary_boys(current, next)
      type(User), pointer                               :: current, tmp, next

      if (Associated(next)) then
         if ( current%birthYear > next%birthYear) then
            tmp => current

            current => next
            deallocate(tmp)
         end if
         call Delete_unnecessary_boys(current, next%next)
      else
         current%next => Null()
      end if
   end subroutine Delete_unnecessary_boys

   pure recursive subroutine Get_list_by_gender(user_inp, list_inp, Amount, Gender)
      type(User), intent(in)           :: user_inp
      type(User), pointer              :: list_inp
      integer, intent(inout)           :: Amount
      character(kind=CH_), intent(in)  :: Gender

      if (user_inp%Gender == Gender) then
         allocate (list_inp, source=user_inp)
         Amount = Amount + 1
         list_inp%next => Null()
         if (Associated(user_inp%next)) &
            call Get_list_by_gender(user_inp%next, list_inp%next, Amount, Gender)
      else if (Associated(user_inp%next)) then
         call Get_list_by_gender(user_inp%next, list_inp, Amount, Gender)

      else
         list_inp => Null()
      end if

   end subroutine Get_list_by_gender


end module Users_Process
