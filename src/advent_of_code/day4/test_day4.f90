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
end module test_day4