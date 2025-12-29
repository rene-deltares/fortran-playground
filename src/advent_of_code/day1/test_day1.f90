module test_day1
    use assertions_gtest
    use m_rotations, only: read_rotations, count_zeroes, &
        MAX_LINES, ERROR_INVALID_DIRECTION, ERROR_READ_FAILED
    use m_error, only: error_t

    implicit none

contains

    !$f90tw TESTCODE(TEST, test_day1, test_read_rotations, test_read_rotations,
    subroutine test_read_rotations() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer :: unit
        integer, allocatable :: rotations(:)
        type(error_t), allocatable :: error

        open(newunit=unit, status="scratch")

        write(unit, "(A)") "L41"
        write(unit, "(A)") "R42"

        rewind(unit)

        call read_rotations(unit, rotations, error)

        close(unit)

        call F90_ASSERT_TRUE(allocated(rotations), "Expected rotations to be allocated" // c_null_char)
        call F90_ASSERT_FALSE(allocated(error), "Expected error not to be initialized" // c_null_char)
        call F90_ASSERT_EQ(size(rotations), 2, "Expected two rotations" // c_null_char)
        call F90_ASSERT_EQ(rotations(1), -41)
        call F90_ASSERT_EQ(rotations(2), 42)
    end subroutine test_read_rotations
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day1, test_read_rotations__empty_file, test_read_rotations__empty_file,
    subroutine test_read_rotations__empty_file() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer :: unit
        integer, allocatable :: rotations(:)
        type(error_t), allocatable :: error

        open(newunit=unit, status="scratch")

        call read_rotations(unit, rotations, error)

        close(unit)

        call F90_ASSERT_TRUE(allocated(rotations), "Expected rotations to be allocated" // c_null_char)
        call F90_ASSERT_FALSE(allocated(error), "Expected error not to be initialized" // c_null_char)
        call F90_ASSERT_EQ(size(rotations), 0, "Expected two rotations" // c_null_char)
    end subroutine test_read_rotations__empty_file
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day1, test_read_rotations__invalid_direction, test_read_rotations__invalid_direction,
    subroutine test_read_rotations__invalid_direction() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer :: unit
        integer, allocatable :: rotations(:)
        type(error_t), allocatable :: error

        open(newunit=unit, status="scratch")

        write(unit, "(A)") "L41"
        write(unit, "(A)") "M42" ! Error: Invalid direction.
        rewind(unit)

        call read_rotations(unit, rotations, error)

        close(unit)

        call F90_ASSERT_FALSE(allocated(rotations), "Expected rotations not to be allocated" // c_null_char)
        call F90_ASSERT_TRUE(allocated(error), "Expected error to be initialized" // c_null_char)
        call F90_ASSERT_EQ(error%code, ERROR_INVALID_DIRECTION)
    end subroutine test_read_rotations__invalid_direction
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day1, test_read_rotations__invalid_ticks, test_read_rotations__invalid_ticks,
    subroutine test_read_rotations__invalid_ticks() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer :: unit
        integer, allocatable :: rotations(:)
        type(error_t), allocatable :: error

        open(newunit=unit, status="scratch")

        write(unit, "(A)") "L41"
        write(unit, "(A)") "R??" ! Error: Invalid ticks.
        rewind(unit)

        call read_rotations(unit, rotations, error)

        close(unit)

        call F90_ASSERT_FALSE(allocated(rotations), "Expected rotations not to be allocated" // c_null_char)
        call F90_ASSERT_TRUE(allocated(error), "Expected error to be initialized" // c_null_char)
        call F90_ASSERT_EQ(error%code, ERROR_READ_FAILED, "Expected read failure" // c_null_char)
    end subroutine test_read_rotations__invalid_ticks
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day1, test_count_zeroes, test_count_zeroes,
    subroutine test_count_zeroes() bind(C)
        implicit none
        call F90_ASSERT_EQ(0, count_zeroes([1, -1], from=50))
        call F90_ASSERT_EQ(1, count_zeroes([1, -1], from=0))
        call F90_ASSERT_EQ(3, count_zeroes([1, -1, 2, -2, 3, -3], from=0))
        call F90_ASSERT_EQ(1, count_zeroes([1, -2, 1, 2], from=0))
        call F90_ASSERT_EQ(1, count_zeroes([-1, -1, -1, -1, -1], from=3))
        call F90_ASSERT_EQ(1, count_zeroes([1, 1, 1, 1, 1], from=97))
    end subroutine test_count_zeroes
    !$f90tw )

end module test_day1