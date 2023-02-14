program lab_1_1
   use Environment

   implicit none
   integer, parameter                       :: USER_AMOUNT = 14, LN_LEN = 15, G_LEN = 1
   character(kind=CH_), parameter           :: MALE = Char(1052, CH_)

   character(:), allocatable                :: input_file, output_file, format
   integer                                  :: In, Out, IO, i, birthYear(USER_AMOUNT) = 0, Boys_Amount = 0, Girls_Amount = 0
   integer                                  :: indexBoy(1), indexGirl(1)

   character(LN_LEN, kind=CH_)              :: Last_Names(USER_AMOUNT) = ""
   character(G_LEN, kind=CH_)               :: Gender(USER_AMOUNT) = ""

   character(LN_LEN, kind=CH_), allocatable :: Last_Names_Boys(:), Last_Names_Girls(:)

   logical, allocatable                     :: Is_A_Boy(:), Is_A_Girl(:)

   integer, allocatable                     :: BoysBirthYear(:), GirlsBirthYear(:), Boys_Pos(:), Girls_Pos(:)

   integer, parameter                       :: INDEXES(*) = [(i, i = 1, USER_AMOUNT)]

   input_file = "../data/input.txt"
   output_file = "output.txt"

   open (file=input_file, encoding=E_, newunit=In)
      format = '(a, i4, a)'
      read (In, format, iostat=IO) (Last_Names(i), birthYear(i), Gender(i), i=1, USER_AMOUNT)
   close (In)

   ! handle read status
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(IO)
      case(0)
      case(IOSTAT_END)
         write (Out, "(a)") "End of file has been reached while reading users file."
      case(1:)
         write (Out, "(a)") "Error while reading class list:", IO
      case default
         write (Out, "(a)") "Undetermined error has been reached while reading users file: ", IO
      end select

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "(a)") "Исходный список:"
      write (Out, format, iostat=IO) (Last_Names(i), birthYear(i), Gender(i), i=1, USER_AMOUNT)
   close (Out)

   ! handle write status
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(IO)
      case(0)
      case(IOSTAT_END)
         write (Out, "(a)") "End of file has been reached while writing users file."
      case(1:)
         write (Out, "(a)") "Error while writing users list:", IO
      case default
         write (Out, "(a)") "Undetermined error has been reached while writing users file: ", IO
   end select

   Is_A_Boy       = Gender == MALE
   Boys_Amount    = Count(Is_A_Boy)

   Boys_Pos   = Pack(INDEXES, Is_A_Boy)
   allocate (Last_Names_Boys(Boys_Amount), BoysBirthYear(Boys_Amount))
   do concurrent (i = 1:Boys_Amount)
      Last_Names_Boys(i)  = Last_Names(Boys_Pos(i))
      BoysBirthYear(i)  = birthYear(Boys_Pos(i))
   end do

   Is_A_Girl      = .not. Is_A_Boy
   Girls_Amount   = USER_AMOUNT - Boys_Amount
   Girls_Pos   = Pack(INDEXES, Is_A_Girl)

   allocate (Last_Names_Girls(Girls_Amount), GirlsBirthYear(Girls_Amount))
   do concurrent (i = 1:Girls_Amount)
      Last_Names_Girls(i)  = Last_Names(Girls_Pos(i))
      GirlsBirthYear(i)  = birthYear(Girls_Pos(i))
   end do

   indexBoy = MINLOC(BoysBirthYear, DIM=1)
   indexGirl = MAXLOC(GirlsBirthYear, DIM=1)


   open(file=output_file, encoding=E_, position="append", newunit=Out)
      write (Out, "(/a)") "Самый пожилой мужчина:"
      write (Out, '(a, i4)', iostat=IO) Last_Names_Boys(indexBoy), BoysBirthYear(indexBoy)
      write (Out, "(/a)") "Самая молодая женщина:"
      write (Out, '(a, i4)', iostat=IO) Last_Names_Girls(indexGirl), GirlsBirthYear(indexGirl)
   close (Out)

   !handle result write status
   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(IO)
      case(0)
      case(IOSTAT_END)
         write (Out, "(a)") "End of file has been reached while writing result users file."
      case(1:)
        write (Out, "(a)") "Error while writing result users list:", IO
      case default
         write (Out, "(a)") "Undetermined error has been reached while whiting result usrs file: ", IO
   end select

end program lab_1_1
