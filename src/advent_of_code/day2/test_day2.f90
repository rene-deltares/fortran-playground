module test_day2
    use iso_fortran_env, only: int32, int64
    use assertions_gtest
    use m_ranges
    use m_error, only: error_t

    implicit none

contains
    !$f90tw TESTCODE(TEST, test_day2, test_read_ranges__single, test_read_ranges__single,
    subroutine test_read_ranges__single() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer :: unit
        type(range_t), allocatable :: ranges(:)
        type(error_t), allocatable :: error

        open(newunit=unit, status="scratch")

        write(unit, "(A)") "12345678-12345679"
        rewind(unit)

        call read_ranges(unit, ranges, error)

        close(unit)

        call F90_ASSERT_TRUE(allocated(ranges), "Expected ranges to be allocated" // c_null_char)
        call F90_ASSERT_FALSE(allocated(error), "Expected error not to be initialized" // c_null_char)
        call F90_ASSERT_EQ(size(ranges), 1, "Expected one range" // c_null_char)
        call F90_ASSERT_EQ(int(ranges(1)%from, kind=int32), 12345678)
        call F90_ASSERT_EQ(int(ranges(1)%to, kind=int32), 12345679)
    end subroutine test_read_ranges__single
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day2, test_read_ranges__trailing_comma, test_read_ranges__trailing_comma,
    subroutine test_read_ranges__trailing_comma() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer :: unit
        type(range_t), allocatable :: ranges(:)
        type(error_t), allocatable :: error

        open(newunit=unit, status="scratch")

        write(unit, "(A)") "12345678-12345679,"
        rewind(unit)

        call read_ranges(unit, ranges, error)

        close(unit)

        call F90_ASSERT_TRUE(allocated(ranges), "Expected ranges to be allocated" // c_null_char)
        call F90_ASSERT_FALSE(allocated(error), "Expected error not to be initialized" // c_null_char)
        call F90_ASSERT_EQ(size(ranges), 1, "Expected one range" // c_null_char)
        call F90_ASSERT_EQ(int(ranges(1)%from, kind=int32), 12345678)
        call F90_ASSERT_EQ(int(ranges(1)%to, kind=int32), 12345679)
    end subroutine test_read_ranges__trailing_comma
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day2, test_read_ranges__multiple, test_read_ranges__multiple,
    subroutine test_read_ranges__multiple() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer :: unit, i
        type(range_t), allocatable :: ranges(:)
        type(error_t), allocatable :: error

        open(newunit=unit, status="scratch")

        write(unit, "(A)") "42-43,44-45,46-47"
        rewind(unit)

        call read_ranges(unit, ranges, error)

        close(unit)

        call F90_ASSERT_TRUE(allocated(ranges), "Expected ranges to be allocated" // c_null_char)
        call F90_ASSERT_FALSE(allocated(error), "Expected error not to be initialized" // c_null_char)
        call F90_ASSERT_EQ(size(ranges), 3, "Expected three ranges" // c_null_char)
        do i = 1, 3
            call F90_ASSERT_EQ(int(ranges(i)%from, kind=int32), 42 + 2 * (i - 1))
            call F90_ASSERT_EQ(int(ranges(i)%to, kind=int32), 43 + 2 * (i - 1))
        end do
    end subroutine test_read_ranges__multiple
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day2, test_is_silly_number, test_is_silly_number,
    subroutine test_is_silly_number() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer(kind=int64), parameter :: values(*) = [11, 22, 99, 1010, 1188511885, 222222, 446446, 38593859]
        integer :: i

        do i = 1, size(values)
            call F90_ASSERT_TRUE(is_extra_silly_number(values(i)), "Expected value to be silly" // c_null_char)
        end do
    end subroutine test_is_silly_number
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day2, test_next_silly_number, test_next_silly_number,
    subroutine test_next_silly_number() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer(kind=int64), parameter :: test(*) = [9, 99, 999, 9999, 123012, 123123, 123124]
        integer(kind=int64), parameter :: outcomes(*) = [11, 1010, 1010, 100100, 123123, 124124, 124124]
        integer :: i, expected, actual

        do i = 1, size(test)
            expected = outcomes(i)
            actual = next_silly_number(test(i))
            call F90_ASSERT_EQ(expected, actual)
        end do
    end subroutine test_next_silly_number
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day2, test_is_extra_silly_number, test_is_extra_silly_number,
    subroutine test_is_extra_silly_number() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer(kind=int64), parameter :: values(*) = [111, 22222, 9999999, 121212, 123123]
        integer :: i
        character(len=120) :: message

        do i = 1, size(values)
            write(message, "(A,I0)") "Expected value to be extra silly: ", values(i)
            call F90_ASSERT_TRUE(is_extra_silly_number(values(i)), trim(message) // c_null_char)
        end do
    end subroutine test_is_extra_silly_number
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day2, test_next_extra_silly_number, test_next_extra_silly_number,
    subroutine test_next_extra_silly_number() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        integer(kind=int64), parameter :: test(*) = [9, 99, 999, 9999, 123012, 123123, 123124]
        integer(kind=int64), parameter :: outcomes(*) = [11, 111, 1010, 11111, 123123, 124124, 124124]
        integer :: i, expected, actual
        character(len=120) :: message

        do i = 1, size(test)
            expected = outcomes(i)
            actual = next_extra_silly_number(test(i))
            write (message, "(A,I0)") "Expected and actual next extra silly number mismatch for: ", test(i)
            call F90_ASSERT_EQ(expected, actual, trim(message) // c_null_char)
        end do
    end subroutine test_next_extra_silly_number
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day2, test_extra_silly_numbers_in_range, test_extra_silly_numbers_in_range,
    subroutine test_extra_silly_numbers_in_range() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        type(range_t), parameter :: ranges(*) = [ &
            range_t(11, 22), range_t(95, 115), range_t(998, 1012), range_t(1188511880, 1188511890), &
            range_t(222220, 222224), range_t(1698522, 1698528), range_t(446443, 446449), &
            range_t(38593856, 38593862), range_t(565653, 565659), range_t(824824821, 824824827), &
            range_t(2121212118, 2121212124) &
        ]
        integer(kind=int64), parameter :: expected_silly_numbers(2, 11) = reshape( &
            [ &
                11, 22, &
                99, 111, &
                999, 1010, &
                1188511885, 0, &
                222222, 0, &
                0, 0, &
                446446, 0, &
                38593859, 0, &
                565656, 0, &
                824824824, 0, &
                2121212121, 0 &
            ], [2, 11] &
        )
        type(range_t) :: current
        integer(kind=int64) :: expected(2)
        integer(kind=int64), allocatable :: actual(:)
        integer :: i, zero_index
        integer :: abs_error

        do i = 1, size(ranges)
            current = ranges(i)
            expected = expected_silly_numbers(:,i)
            actual = extra_silly_numbers_in_range(current)

            abs_error = sum(abs(expected(1:size(actual)) - actual))
            call F90_ASSERT_EQ(abs_error, 0)
        end do
    end subroutine test_extra_silly_numbers_in_range
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day2, test_silly_number_sum, test_silly_number_sum,
    subroutine test_silly_number_sum() bind(C)
        use iso_c_binding, only: c_null_char
        implicit none
        type(range_t), parameter :: ranges(*) = [ &
            range_t(11, 22), range_t(95, 115), range_t(998, 1012), range_t(1188511880, 1188511890), &
            range_t(222220, 222224), range_t(1698522, 1698528), range_t(446443, 446449), &
            range_t(38593856, 38593862), range_t(565653, 565659), range_t(824824821, 824824827), &
            range_t(2121212118, 2121212124) &
        ]
        integer, parameter :: silly_number_sums(*) = [ &
            33, 99, 1010, 1188511885, 222222, 0, 446446, 38593859, 0, 0, 0 &
        ]
        integer :: expected, actual, i

        do i = 1, size(ranges)
            expected = silly_number_sums(i)
            actual = silly_number_sum(ranges(i))
            call F90_ASSERT_EQ(expected, actual)
        end do
    end subroutine test_silly_number_sum
    !$f90tw )
end module test_day2