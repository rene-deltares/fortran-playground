module test_example
    
   use assertions_gtest
   use example

   implicit none
 
   real(4), parameter :: delta = 0.000001

   contains

   !$f90tw TESTCODE(TEST, test_gtest, test_example1, add_test_method,
   subroutine add_test_method() BIND(C, name="add_test_method")
      use ISO_C_BINDING, only: C_CHAR, C_NULL_CHAR
      character(KIND=C_CHAR, LEN=*), parameter :: message = "delta=0.000001 !" // C_NULL_CHAR
      call F90_ASSERT_EQ(method(2, 1), 3)
      call F90_ASSERT_EQ(method(2, 2), 3, "method(2,1) == 3" // C_NULL_CHAR )
      call F90_ASSERT_FLOAT_EQ(1.0, 1.0 + delta, message)
   end subroutine add_test_method
   !$f90tw )

end module test_example
 