module example

      implicit none

      interface method
          module procedure add_i4
          module procedure add_r4
          module procedure add_r8
      end interface

      contains

      integer(KIND=4) function add_i4(i,j)
          integer(KIND=4), intent (in) :: i, j
          add_i4=i+j
      end function add_i4

      real(KIND=4) function add_r4(a,b)
          real(KIND=4), intent (in) :: a, b
          add_r4=a+b
      end function add_r4

      real(KIND=8) function add_r8(a,b)
          real(KIND=8), intent (in) :: a, b
          add_r8=a+b
      end function add_r8

end module
