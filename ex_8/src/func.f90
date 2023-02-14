module Func
   use Environment
   use Func_IO

   implicit none

contains
   function Matrix_norm(A) result(mx_norm)
      real(R_)              :: A(:, :), mx_norm

      intent(in)     A

      mx_norm = Matrix_norm_2(A)
      if (mx_norm < 1) mx_norm = Matrix_norm_3(A)

   end function Matrix_norm

   real(R_) function Matrix_norm_3(A)
      integer               :: N
      real(R_)              :: A(:, :)
      real(R_), allocatable :: res(:, :)

      intent(in)  A
      
      N = Ubound(A, 1)
      allocate (res(N, N))

      Matrix_norm_3 = 0

      res = Multiply_matrix(A, Multiply_matrix(A, A)) + 1

      Matrix_norm_3 = Matrix_norm_2(res)

   end function Matrix_norm_3

   function Multiply_matrix(A, B) result(res)
         real(R_), intent(in)      :: A(:, :), B(:, :)
         real(R_), allocatable     :: res(:, :)
         real(R_)                  :: s
         integer                   :: i, j, k, N

         N = Ubound(A, 1)
         allocate (res(N, N))

         DO i=1,N
            DO j=1,N
            s= .0
               DO k=1,N
                  S=s+A(i,k)*B(k,j)
               END DO
            res(i,j)=s
            END DO
         END DO

   end function Multiply_matrix

   real(R_) function Matrix_norm_2(A)
      integer                        :: i, N
      real(R_)                       :: A(:, :)
      real(R_), allocatable          :: Sums(:)

      intent(in)  A
      
      Matrix_norm_2 = 0
      N = Ubound(A, 1)
      allocate (Sums(N), source=0.0)

      Sums(:) = [(SUM(A(:, I)), I=1, N)]
      Matrix_norm_2 = MAXVAL(Sums(:))

   end function Matrix_norm_2
   
end module Func
