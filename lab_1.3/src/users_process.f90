module Users_Process
   use Environment
   use Users_IO

   implicit none
   
contains
   pure function Handle_Users_List(Usrs) result(Usrs_Res)
      type(Users)              :: Usrs(:)
      type(Users)              :: Usrs_Res(2)
      integer                  :: indexOfBoy(1), indexOfGirl(1)
      character(kind=CH_)      :: MALE

      intent (in)              Usrs

      MALE = Char(1052, CH_)
      indexOfBoy     = MINLOC(Usrs%birthYear, MASK = (Usrs%Gender == MALE), DIM=1)
      indexOfGirl    = MAXLOC(Usrs%birthYear, MASK = (.not. Usrs%Gender == MALE), DIM=1)

      Usrs_Res(:1) = Usrs(indexOfBoy)
      Usrs_Res(2:2) = Usrs(indexOfGirl)
   end function Handle_Users_List
end module Users_Process
