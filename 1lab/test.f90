program sample
        implicit none
        integer :: a, b
        print*, "Введите a"
        read(*,*) a
        print*, "Введите b"
        read(*,*) b
        a=a+b
        b=a-b
        a=a-b 
        write(*,*) "a = ", a
        write(*,*) "b = ", b
end program sample
