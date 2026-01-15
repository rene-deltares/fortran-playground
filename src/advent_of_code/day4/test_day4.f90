module test_day4
    use iso_c_binding, only: c_null_char
    use assertions_gtest
    use m_error, only: error_t
    use m_scrolls
    implicit none
contains
    !$f90tw TESTCODE(TEST, test_day4, test_read_scrolls, test_read_scrolls,
    subroutine test_read_scrolls() bind(C)
        implicit none
        integer :: unit, rows, columns
        logical, allocatable :: grid(:,:)
        type(error_t), allocatable :: error

        ! Arrange
        open(newunit=unit, status="scratch")
        write(unit, "(A)") ".@.@.@.@."
        write(unit, "(A)") "@.@.@.@.@"
        write(unit, "(A)") ".@.@.@.@."
        rewind(unit)

        ! Act
        call read_scrolls(unit, grid, rows, columns, error)
        close(unit)

        ! Assert
        call F90_ASSERT_FALSE(allocated(error))
        call F90_ASSERT_TRUE(allocated(grid))
        call F90_ASSERT_EQ(rows, 5, "Expected 5 rows" // c_null_char)
        call F90_ASSERT_EQ(columns, 11, "Expected 11 columns" // c_null_char)
        call F90_ASSERT_TRUE(grid(:, 1) == .false., "Expected first column to be all false" // c_null_char)
        call F90_ASSERT_TRUE(grid(:, columns) == .false., "Expected last column to be all false" // c_null_char)
        call F90_ASSERT_TRUE(grid(1, :) == .false., "Expected first row to be all false" // c_null_char)
        call F90_ASSERT_TRUE(grid(rows, :) == .false., "Expected last row to be all false" // c_null_char)
    end subroutine test_read_scrolls
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day4, test_read_scrolls_row__column_mismatch, test_read_scrolls__column_mismatch,
    subroutine test_read_scrolls__column_mismatch() bind(C)
        implicit none
        integer :: unit, rows, columns
        logical, allocatable :: grid(:,:)
        type(error_t), allocatable :: error

        ! Arrange
        open(newunit=unit, status="scratch")
        write(unit, "(A)") ".@.@.@.@."
        write(unit, "(A)") "@.@.@.@.@."
        rewind(unit)

        ! Act
        call read_scrolls(unit, grid, rows, columns, error)
        close(unit)

        ! Assert
        call F90_ASSERT_TRUE(allocated(error))
        call F90_ASSERT_FALSE(allocated(grid))
        call F90_ASSERT_EQ(error%code, ERROR_INVALID_INPUT)
    end subroutine test_read_scrolls__column_mismatch
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day4, test_accessible_scrolls, test_accessible_scrolls,
    subroutine test_accessible_scrolls() bind(C)
        implicit none
        logical :: grid(5, 5)
        integer, allocatable :: coordinates(:, :)
        integer :: i

        grid = .false.
        grid(2:4, 2:4) = .true.
        grid(3,3) = .false.

        call accessible_scrolls(grid, coordinates)

        call F90_ASSERT_TRUE(allocated(coordinates))
        call F90_ASSERT_EQ(size(coordinates, 2), 4, "Expected 4 accessible scrolls" // c_null_char)
        call F90_ASSERT_TRUE(any([(coordinates(:, i) == [2, 2], i = 1, 4)]), "Expected [2, 2] to be accessible" // c_null_char)
        call F90_ASSERT_TRUE(any([(coordinates(:, i) == [2, 4], i = 1, 4)]), "Expected [2, 4] to be accessible" // c_null_char)
        call F90_ASSERT_TRUE(any([(coordinates(:, i) == [4, 2], i = 1, 4)]), "Expected [2, 4] to be accessible" // c_null_char)
        call F90_ASSERT_TRUE(any([(coordinates(:, i) == [4, 4], i = 1, 4)]), "Expected [2, 4] to be accessible" // c_null_char)
    end subroutine test_accessible_scrolls
    !$f90tw )

    !$f90tw TESTCODE(TEST, test_day4, test_remove_scrolls, test_remove_scrolls,
    subroutine test_remove_scrolls() bind(C)
        implicit none
        logical :: grid(3, 3)
        integer :: i
        integer, parameter :: coordinates(2, 3) = [[1, 1], [2, 2], [3, 3]]

        grid = .true.

        call remove_scrolls(grid, coordinates)

        call F90_ASSERT_TRUE( &
            .not. any([(grid(i, i), i = 1, 3)]), &
            "Expected scrolls on diagonal to be removed" // c_null_char &
        )
        call F90_ASSERT_EQ(6, sum(merge(1, 0, grid)), "Expected all off-diagonal elements to remain.")
    end subroutine test_remove_scrolls
    !$f90tw )
end module test_day4