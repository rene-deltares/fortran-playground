module test_day5
    use iso_fortran_env, only: int64
    use assertions_gtest
    use m_error, only: error_t
    use m_inventory
    implicit none
contains

    !$f90tw TESTCODE(TEST, test_day5, test_read_inventory, test_read_inventory,
    subroutine test_read_inventory() bind(C)
        implicit none

        type(error_t), allocatable :: error
        type(inventory_t), allocatable :: inventory
        integer :: unit

        ! Arrange
        open(unit, status="scratch")
        write(unit, "(A)") "123456789101112-123456789101113"
        write(unit, "(A)") "123456789101114-123456789101115"
        write(unit, "()")
        write(unit, "(A)") "123456789101112"
        write(unit, "(A)") "123456789101116"
        rewind(unit)

        ! Act
        call read_inventory(unit, inventory, error)
        close(unit)

        ! Assert
        call F90_ASSERT_FALSE(allocated(error), "Expected no error" // c_null_char)
        call F90_ASSERT_TRUE(allocated(inventory), "Expected inventory to be read" // c_null_char)
        call F90_ASSERT_EQ(size(inventory%ranges), 1, "Expected one range in inventory" // c_null_char)
        call F90_ASSERT_EQ(size(inventory%items), 2, "Expected two items in inventory" // c_null_char)

        call F90_ASSERT_TRUE(inventory%ranges(1)%begin == 123456789101112_int64)
        call F90_ASSERT_TRUE(inventory%ranges(1)%end == 123456789101115_int64)
        call F90_ASSERT_TRUE(inventory%items(1) == 123456789101112_int64)
        call F90_ASSERT_TRUE(inventory%items(2) == 123456789101116_int64)
    end subroutine test_read_inventory
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day5, test_sort_ranges, test_sort_ranges,
    subroutine test_sort_ranges() bind(C)
        implicit none
        type(range_t), allocatable :: result(:)
        type(range_t), allocatable :: expected(:)

        result = sort_ranges( &
            [range_t(5, 10), range_t(4, 8), range_t(14, 42), &
            range_t(5, 12), range_t(4, 7), range_t(14, 42)] &
        )

        expected = [ &
            range_t(4, 7), range_t(4, 8), range_t(5, 10), &
            range_t(5, 12), range_t(14, 42), range_t(14, 42) &
        ]

        call F90_ASSERT_EQ(size(result), size(expected))
        call F90_ASSERT_TRUE(all(result == expected))
    end subroutine test_sort_ranges
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day5, test_squash_ranges, test_squash_ranges,
    subroutine test_squash_ranges() bind(C)
        implicit none
        type(range_t), allocatable :: result(:)
        type(range_t), allocatable :: expected(:)

        result = squash_ranges( &
            [range_t(5, 10), range_t(11, 15), range_t(17, 20), &
            range_t(18, 25), range_t(19, 24)] &
        )

        expected = [range_t(5, 15), range_t(17, 25)]

        call F90_ASSERT_EQ(size(result), size(expected))
        call F90_ASSERT_TRUE(all(result == expected))
    end subroutine test_squash_ranges
    !$f90tw )

end module test_day5